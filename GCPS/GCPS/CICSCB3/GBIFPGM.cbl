00001  IDENTIFICATION DIVISION.                                         09/29/04
00002  PROGRAM-ID.     GBIFPGM.                                         GBIFPGM 
00003  AUTHOR.         G. PEREZ.                                           LV002
00004  DATE-WRITTEN.   02/02/01.                                        GBIFPGM 
00005  DATE-COMPILED.                                                   GBIFPGM 
00006 ******************************************************************GBIFPGM 
00007 *                  G B I F P G M                                  GBIFPGM 
00008 *                                                                *GBIFPGM 
00009 *    GBIFPGM   BENEFIT HIGHLIGHTS PROGRAM                        *GBIFPGM 
00010 *                                                                *GBIFPGM 
00011 *    THIS PROGRAM WILL DETERMINE RULE VALUES AND RETURN THEM     *GBIFPGM 
00012 *    TO THE CALLING PROGRAM.                                     *GBIFPGM 
00013 *                                                                *GBIFPGM 
00014 *                                                                *GBIFPGM 
00015 *   FUNC CODE: GBIF                                              *GBIFPGM 
00016 * INPUT FILES: GROUP FILE                                        *GBIFPGM 
00017 *              CONTRACT FILE                                     *GBIFPGM 
00018 *              TABULAR FILE                                      *GBIFPGM 
00019 *                                                                *GBIFPGM 
00020 ******************************************************************GBIFPGM 
00021 *                                                                *GBIFPGM 
00022 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GBIFPGM 
00023 *       *-*         U P D A T E   H I S T O R Y         *-*      *GBIFPGM 
00024 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GBIFPGM 
00025 *                                                                *GBIFPGM 
00026 **-CHG-NUM-* *-DATE-* *WHO* *-----DESCRIPTION--------*            GBIFPGM 
00027 *                                                                *GBIFPGM 
00028 *    XXXX    02/02/01  GSP  CREATED SKELETON PROGRAM.            *GBIFPGM 
00029 *                                                                 GBIFPGM 
00030 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GBIFPGM 
00031 *                                                                *GBIFPGM 
00032 *            10/23/02   JP  CHANGED DISPLAY OF LIFETIME MAX      *GBIFPGM 
00033 *                           FOR WEB BHS; FIXED CODE FOR FODD     *GBIFPGM 
00034 *                           & FOOP.                              *GBIFPGM 
00035 *                                                                *GBIFPGM 
00036 *            1/6/03     JP  CODE DEFAULT VALUES PARAGRAPH        *GBIFPGM 
00037 *                           AFTER SOME ACCUMID'S DELETED FROM    *GBIFPGM 
00038 *                           PROTOTYPES EFF 1/1/03.               *GBIFPGM 
00039 *                                                                *GBIFPGM 
00040 *            3/20/03    JP  ADD UTILIZATION FIELDS               *GBIFPGM 
00041 *                                                                *GBIFPGM 
00042 *            5/30/03    JP  CHANGES PER SSD                      *GBIFPGM 
00043 *                                                                *GBIFPGM 
00044 *            6/05/03    MQ  MAKE CHANGES TO SPECIFIC ACCUM IDS   *GBIFPGM 
00045 *                           PER SSD INSTRUCTIONS                 *GBIFPGM 
00046 *                                                                *GBIFPGM 
00047 *            10/09/03   MQ  ADD NEW RULE VALUES PER SSD          *GBIFPGM 
00048 *                                                                *GBIFPGM 
00049 *            10-23-03   KDM   RECOMPILE FOR PRIME                *GBIFPGM 
00050 *                                                                *GBIFPGM 
00051 *            12/03/03   MQ  MODIFY LOGIC FOR HOSP PYMT LVL (IPH) *GBIFPGM 
00052 *                           & MED SURG PYMT LVL (MSP)            *GBIFPGM 
00053 *                                                                *GBIFPGM 
00054 *            02/13/04   MQ  PARA 6000- DELETE WCPI DEFAULT       *GBIFPGM 
00055 *                           VALUE.  IN PARA 4340- DISPLAY        *GBIFPGM 
00056 *                           MSAC DIFFERENCE INSTEAD OF           *GBIFPGM 
00057 *                           REDUCTION AMOUNT.                    *GBIFPGM 
00058 *                                                                *GBIFPGM 
00059 *            03/03/04   MQ  CHANGES PER SSD                      *GBIFPGM 
00060 *                                                                *GBIFPGM 
00061 *            03/23/04   JP  PROD FIX FOR BAE ALTERNATIVE GROUPS  *GBIFPGM 
00062 *                                                                *GBIFPGM 
00063 *            04/07/04   MQ  PER SSD INSTRUCTIONS:                *GBIFPGM 
00064 *                           DELETE DEFAULT LOGIC FOR IPHO/MSPO   *GBIFPGM 
00065 *                           CODE NEW ACCUM ID GLAA (VISION)      *GBIFPGM 
00066 *                           MODIFY MSAD ACCUM ID LOGIC TO LOOK   *GBIFPGM 
00067 *                           ON BOTH ADL & AOL TAB                *GBIFPGM 
00068 *                                                                *GBIFPGM 
00069 *            05/13/04   MQ  ADD LOGIC FOR TEXAS                  *GBIFPGM 
00070 *                                                                *GBIFPGM 
00071 *            05/17/04   MQ  CHANGE VALUE QUALIFIER LOGIC         *GBIFPGM 
00072 *                           IN PARA 4290-                        *GBIFPGM 
00073 *                                                                *GBIFPGM 
00074 *            01/13/05   MQ  ADD LOGIC FOR TEXAS ACCUM IDS        *GBIFPGM 
00075 *                                                                *GBIFPGM 
00076 *            01/24/05   MQ  ADD LOGIC FOR TX STAND ALONE         *GBIFPGM 
00077 *                           DEDL/OOP ACCUM IDS                   *GBIFPGM 
00078 *                                                                *GBIFPGM 
00079 *            05/03/05   MQ  ADD NEW RULE 4115 (PER ADM DEDL MAX) *GBIFPGM 
00080 *                           AND ACCUM IDS MSID/MSOD TO RULE 4350 *GBIFPGM 
00081 *                                                                *GBIFPGM 
00082 *            05/11/05   MQ  ADD NEW ACCUM IDS FOR TEXAS ERS/HMO  *GBIFPGM 
00083 *                           COMMENT OUT LOGIC FOR RULE 4930- HAD *GBIFPGM 
00084 *                           SAME ACCUM IDS AS RULE 5070          *GBIFPGM 
00073 *                                                                *GBIFPGM 
00073 *            08/15/24   NSK ADDED BACK MISSING FIELDS FROM 2004  *GBIFPGM 
00073 *                           TO 2025 TO FIX BBDA-5950 DEFECT TO   *GBIFPGM 
00073 *                           FIELDS CORRECTLY IN GHIL SCREEN.     *GBIFPGM 
00073 *                                                                *GBIFPGM 
00074 ******************************************************************GBIFPGM 
00075 /                                                                 GBIFPGM 
00076  ENVIRONMENT DIVISION.                                            GBIFPGM 
00077  DATA DIVISION.                                                   GBIFPGM 
00078  WORKING-STORAGE SECTION.                                         GBIFPGM 
00079  77  PAN-VALET PICTURE X(24) VALUE '154GBIFPGMTS 02/02/01'.       GBIFPGM 
00080  77  PAN-DSN   PICTURE  X(44) VALUE                               GBIFPGM 
00081      'HCMSGEN.TEST.PANLIB                         '.              GBIFPGM 
00082  01  WS-DIAGNOSTICS.                                              GBIFPGM 
00083      05  WS-BEGIN                PIC X(20) VALUE                  GBIFPGM 
00084      '**GBIFPGM WS BEGIN**'.                                      GBIFPGM 
00085      05  WS-PARA-ID              PIC X(4)  VALUE 'GBIF'.          GBIFPGM 
00086      05  WS-ABEND-CODE           PIC X(4)  VALUE 'GBIF'.          GBIFPGM 
00087                                                                   GBIFPGM 
00088 ** WORKFIELDS AND SWITCHES **                                     GBIFPGM 
00089  01  WS-WORK-FIELDS.                                              GBIFPGM 
00090      05  WS-HEX-00               PIC X.                           GBIFPGM 
00091      05  WS-PROCESS-CON-GRP-SW   PIC X     VALUE SPACE.           GBIFPGM 
00092          88  WS-PROCESS-CON                VALUE 'C'.             GBIFPGM 
00093          88  WS-PROCESS-GRP                VALUE 'G'.             GBIFPGM 
00094      05  WS-GRP-ABM-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00095          88  WS-GRP-ABM-FOUND              VALUE 'Y'.             GBIFPGM 
00096      05  WS-GRP-ACL-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00097          88  WS-GRP-ACL-FOUND              VALUE 'Y'.             GBIFPGM 
00098      05  WS-GRP-ACP-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00099          88  WS-GRP-ACP-FOUND              VALUE 'Y'.             GBIFPGM 
00100      05  WS-GRP-ADL-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00101          88  WS-GRP-ADL-FOUND              VALUE 'Y'.             GBIFPGM 
00102      05  WS-GRP-AOL-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00103          88  WS-GRP-AOL-FOUND              VALUE 'Y'.             GBIFPGM 
00104      05  WS-CON-ABM-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00105          88  WS-CON-ABM-FOUND              VALUE 'Y'.             GBIFPGM 
00106      05  WS-CON-ACL-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00107          88  WS-CON-ACL-FOUND              VALUE 'Y'.             GBIFPGM 
00108      05  WS-CON-ACP-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00109          88  WS-CON-ACP-FOUND              VALUE 'Y'.             GBIFPGM 
00110      05  WS-CON-ADL-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00111          88  WS-CON-ADL-FOUND              VALUE 'Y'.             GBIFPGM 
00112      05  WS-CON-AOL-FOUND-SW     PIC X     VALUE 'N'.             GBIFPGM 
00113          88  WS-CON-AOL-FOUND              VALUE 'Y'.             GBIFPGM 
00114      05  WS-ERCI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00115          88  ERCI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00116          88  ERCI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00117      05  WS-ERCO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00118          88  ERCO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00119          88  ERCO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00120      05  WS-ERCA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00121          88  ERCA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00122          88  ERCA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00123      05  WS-OVCI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00124          88  OVCI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00125          88  OVCI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00126      05  WS-OVCO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00127          88  OVCO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00128          88  OVCO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00129      05  WS-OVCA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00130          88  OVCA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00131          88  OVCA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00132      05  WS-WCCI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00133          88  WCCI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00134          88  WCCI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00135      05  WS-WCCO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00136          88  WCCO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00137          88  WCCO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00138      05  WS-WCCA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00139          88  WCCA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00140          88  WCCA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00141      05  WS-SOVI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00142          88  SOVI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00143          88  SOVI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00144      05  WS-SOVO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00145          88  SOVO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00146          88  SOVO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00147      05  WS-SOVA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00148          88  SOVA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00149          88  SOVA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00150      05  WS-SCOI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00151          88  SCOI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00152          88  SCOI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00153      05  WS-SCOO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00154          88  SCOO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00155          88  SCOO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00156      05  WS-SCOA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00157          88  SCOA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00158          88  SCOA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00159      05  WS-MSCI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00160          88  MSCI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00161          88  MSCI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00162      05  WS-MSCO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00163          88  MSCO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00164          88  MSCO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00165      05  WS-MSCA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00166          88  MSCA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00167          88  MSCA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00168      05  WS-UCCI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00169          88  UCCI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00170          88  UCCI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00171      05  WS-UCCO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00172          88  UCCO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00173          88  UCCO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00174      05  WS-UCCA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00175          88  UCCA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00176          88  UCCA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00177      05  WS-OPFI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00178          88  OPFI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00179          88  OPFI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00180      05  WS-OPFO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00181          88  OPFO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00182          88  OPFO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00183      05  WS-OPFA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00184          88  OPFA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00185          88  OPFA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00186      05  WS-UCPI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00187          88  UCPI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00188          88  UCPI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00189      05  WS-UCPO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00190          88  UCPO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00191          88  UCPO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00192      05  WS-UCPA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00193          88  UCPA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00194          88  UCPA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00207      05  WS-MSID-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00208          88  MSID-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00209          88  MSID-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00210      05  WS-MSOD-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00211          88  MSOD-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00212          88  MSOD-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00195      05  WS-MSAD-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00196          88  MSAD-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00197          88  MSAD-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00216      05  WS-ACOI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00217          88  ACOI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00218          88  ACOI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00219      05  WS-ACOO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00220          88  ACOO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00221          88  ACOO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00222      05  WS-ACOA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00223          88  ACOA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00224          88  ACOA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00225      05  WS-SNFI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00226          88  SNFI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00227          88  SNFI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00228      05  WS-SNFO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00229          88  SNFO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00230          88  SNFO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00231      05  WS-SNFA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00232          88  SNFA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00233          88  SNFA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00234      05  WS-MHCI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00235          88  MHCI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00236          88  MHCI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00237      05  WS-MHCO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00238          88  MHCO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00239          88  MHCO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00240      05  WS-MHCA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00241          88  MHCA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00242          88  MHCA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00243      05  WS-PACI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00244          88  PACI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00245          88  PACI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00246      05  WS-PACO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00247          88  PACO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00248          88  PACO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00249      05  WS-PACA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00250          88  PACA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00251          88  PACA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00252      05  WS-HCPI-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00253          88  HCPI-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00254          88  HCPI-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00255      05  WS-HCPO-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00256          88  HCPO-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00257          88  HCPO-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00258      05  WS-HCPA-VALQUAL5-SW     PIC X     VALUE 'N'.             GBIFPGM 
00259          88  HCPA-VALQUAL5-SW-ON           VALUE 'Y'.             GBIFPGM 
00260          88  HCPA-VALQUAL5-SW-OFF          VALUE 'N'.             GBIFPGM 
00198      05  WS-TEST-PRODUCT-TYPE                 PIC X(09).          GBIFPGM 
00199          88 WS-BLUALT-GRP    VALUE  'BAE008771'.                  GBIFPGM 
00200                                                                   GBIFPGM 
00201      05  WS-SYSID.                                                GBIFPGM 
00202          10  FILLER                           PIC X(01).          GBIFPGM 
00203              88  WS-TEXAS-CICS-REGION         VALUE 'X'.          GBIFPGM 
00204              88  WS-IL-CICS-REGION            VALUE 'I'.          GBIFPGM 
00205              88  WS-NM-CICS-REGION            VALUE 'N'.          GBIFPGM 
00206          10  FILLER                           PIC X(03).          GBIFPGM 
00207                                                                   GBIFPGM 
00208                                                                   GBIFPGM 
00209      05  WS-WRK-VAL-1    COMP-3  PIC S9(7)V99 VALUE ZEROS.        GBIFPGM 
00210      05  WS-WRK-VAL-2    COMP-3  PIC S9(7)V99 VALUE ZEROS.        GBIFPGM 
00211      05  WS-WRK-VAL-DISP         PIC  Z(7)9.99 VALUE ZEROS.       GBIFPGM 
00212      05  WS-WRK-VAL-BUX       REDEFINES WS-WRK-VAL-DISP           GBIFPGM 
00213                                  PIC  $(7)9.99.                   GBIFPGM 
00214      05  WS-SAVE-IADD    COMP-3  PIC S9(7)V99 VALUE ZEROS.        GBIFPGM 
00215      05  WS-SAVE-FADD    COMP-3  PIC S9(7)V99 VALUE ZEROS.        GBIFPGM 
00216                                                                   GBIFPGM 
00217      05  WS-VALUE-LIMIT          PIC  Z(7)9.99 VALUE ZEROS.       GBIFPGM 
00218      05  WS-VALUE-LIMIT-S     REDEFINES WS-VALUE-LIMIT            GBIFPGM 
00219                                  PIC  $(7)9.99.                   GBIFPGM 
00220                                                                   GBIFPGM 
00221      05  WS-VALUE-LIMIT-MIL    PIC  $Z,ZZZ,ZZ9.                   GBIFPGM 
00222                                                                   GBIFPGM 
00223      05  WS-VALUE-LIMIT-V        PIC  Z(7)9V99 VALUE ZEROS.       GBIFPGM 
00224      05  WS-VALUE-LIMIT-FULL  REDEFINES WS-VALUE-LIMIT-V          GBIFPGM 
00225                                  PIC  Z(9)9.                      GBIFPGM 
00226                                                                   GBIFPGM 
00227      05  WS-TEMP-VALUE-LIMIT  PIC  X(11) VALUE SPACES.            GBIFPGM 
00228                                                                   GBIFPGM 
00229      05  WS-COINS-PERCENT-LEVEL-1  COMP-3  PIC S9(3) VALUE ZEROS. GBIFPGM 
00230      05  WS-COINS-PERCENT-LEVEL-2  COMP-3  PIC S9(3) VALUE ZEROS. GBIFPGM 
00231                                                                   GBIFPGM 
00232      05  WS-PERCENT.                                              GBIFPGM 
00233          10  FILLER           PIC X(6)  VALUE SPACES.             GBIFPGM 
00234          10  WS-PERCENT-VAL   PIC Z(2)9 VALUE ZEROS.              GBIFPGM 
00235          10  WS-PERCENT-SIGN  PIC X     VALUE '%'.                GBIFPGM 
00236                                                                   GBIFPGM 
00237      05  WS-PEOPLE.                                               GBIFPGM 
00238          10  FILLER           PIC X(01) VALUE SPACES.             GBIFPGM 
00239          10  WS-PEOPLE-VAL    PIC Z(2)9 VALUE ZEROS.              GBIFPGM 
00240          10  FILLER           PIC X(07) VALUE ' PEOPLE'.          GBIFPGM 
00241                                                                   GBIFPGM 
00242      05  WS-VISITS.                                               GBIFPGM 
00243 ******** 10  WS-VISITS-VAL    PIC Z(8)9 VALUE ZEROS.              GBIFPGM 
00244 ******** 10  FILLER           PIC X(02) VALUE ' V'.               GBIFPGM 
00245          10  WS-VISITS-VAL    PIC Z(3)9 VALUE ZEROS.              GBIFPGM 
00246          10  FILLER           PIC X(07) VALUE ' VISITS'.          GBIFPGM 
00247                                                                   GBIFPGM 
00248      05  WS-DAYS.                                                 GBIFPGM 
00249 ******** 10  WS-DAYS-VAL      PIC Z(8)9 VALUE ZEROS.              GBIFPGM 
00250 ******** 10  FILLER           PIC X(02) VALUE ' D'.               GBIFPGM 
00251          10  WS-DAYS-VAL      PIC Z(5)9 VALUE ZEROS.              GBIFPGM 
00252          10  FILLER           PIC X(05) VALUE ' DAYS'.            GBIFPGM 
00253                                                                   GBIFPGM 
00254 *MQ 10/03                                                         GBIFPGM 
00255      05  WS-CONFINEMENTS.                                         GBIFPGM 
00256          10  WS-CONFINEMENTS-VAL  PIC Z(5)9 VALUE ZEROS.          GBIFPGM 
00257          10  FILLER               PIC X(13) VALUE ' CONFINEMENTS'.GBIFPGM 
00258                                                                   GBIFPGM 
00259      05  WS-AGE               PIC 9(03) VALUE ZEROS.              GBIFPGM 
00260                                                                   GBIFPGM 
00261      05  WS-VALUE-LIMIT-DEC   PIC S9(7)V99 VALUE ZEROS.           GBIFPGM 
00262      05  WS-VALUE-LIM REDEFINES WS-VALUE-LIMIT-DEC.               GBIFPGM 
00263          10  WS-VALUE-LIM-1-6 PIC S9(6).                          GBIFPGM 
00264          10  WS-VALUE-LIM-5-7 PIC  9(3).                          GBIFPGM 
00265                                                                   GBIFPGM 
00266      05  WS-HOLD-SECTION-LINE.                                    GBIFPGM 
00267          10  WS-HOLD-SECTION-IND  PIC X(03) VALUE SPACES.         GBIFPGM 
00268          10  FILLER               PIC X(76) VALUE SPACES.         GBIFPGM 
00269                                                                   GBIFPGM 
00270 /                                                                 GBIFPGM 
00271  01  WS-01-ABEND-AREA.                                            GBIFPGM 
00272      05  FILLER                   PIC X(16)  VALUE                GBIFPGM 
00273          '** ABEND AREA **'.                                      GBIFPGM 
00274                                                                   GBIFPGM 
00275      05  WS-01-ABEND-CODES-AND-MSG.                               GBIFPGM 
00276          10  FILLER      PIC X(11)  VALUE  'ABEND-CODE='.         GBIFPGM 
00277          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. GBIFPGM 
00278          10  FILLER      PIC X(10)  VALUE  'ABEND-MSG='.          GBIFPGM 
00279          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. GBIFPGM 
00280          10  FILLER      PIC X(12)  VALUE  'ABEND-MSG-2='.        GBIFPGM 
00281          10  WS-01-ABCODE-MSG-2         PIC X(100) VALUE  SPACES. GBIFPGM 
00282          10  FILLER      PIC X(20)  VALUE  '** ABEND AREA END **'.GBIFPGM 
00283                                                                   GBIFPGM 
00284          10  WS-01-ABCODE-1BS1          PIC X(04)  VALUE  '1BS1'. GBIFPGM 
00285          10  WS-01-ABCODE-1BS1-MSG      PIC X(44)  VALUE          GBIFPGM 
00286             'READQ (INVALID REQUEST)                  '.          GBIFPGM 
00287                                                                   GBIFPGM 
00288          10  WS-01-ABCODE-1BS2          PIC X(04)  VALUE  '1BS2'. GBIFPGM 
00289          10  WS-01-ABCODE-1BS2-MSG      PIC X(44)  VALUE          GBIFPGM 
00290             'READQ (I/O ERROR)                        '.          GBIFPGM 
00291                                                                   GBIFPGM 
00292          10  WS-01-ABCODE-1BS3          PIC X(04)  VALUE  '1BS3'. GBIFPGM 
00293          10  WS-01-ABCODE-1BS3-MSG      PIC X(44)  VALUE          GBIFPGM 
00294             'READQ (LENGTH ERROR)                     '.          GBIFPGM 
00295                                                                   GBIFPGM 
00296          10  WS-01-ABCODE-1BS9          PIC X(04)  VALUE  '1BS9'. GBIFPGM 
00297          10  WS-01-ABCODE-1BS9-MSG      PIC X(44)  VALUE          GBIFPGM 
00298             'READQ  (UNKNOWN RETURN CODE)             '.          GBIFPGM 
00299                                                                   GBIFPGM 
00300 /                                                                 GBIFPGM 
00301  01  WS-LIT-RETURN-CODE-VALUES.                                   GBIFPGM 
00302      05  WS-LIT-RETURN-CODE-CLEAN        PIC X(02) VALUE '00'.    GBIFPGM 
00303      05  WS-LIT-RETURN-REC-NOT-FOUND     PIC X(02) VALUE '01'.    GBIFPGM 
00304      05  WS-LIT-RETURN-EOF-BROWSE        PIC X(02) VALUE '02'.    GBIFPGM 
00305      05  WS-LIT-RETURN-PAGE-IS-FULL      PIC X(02) VALUE '04'.    GBIFPGM 
00306                                                                   GBIFPGM 
00307  01  WS-RETURN-CODE                      PIC X(02) VALUE '00'.    GBIFPGM 
00308      88  WS-RETURN-CODE-CLEAN                      VALUE '00'.    GBIFPGM 
00309      88  WS-RETURN-REC-NOT-FOUND                   VALUE '01'.    GBIFPGM 
00310      88  WS-RETURN-EOF-BROWSE                      VALUE '02'.    GBIFPGM 
00311      88  WS-RETURN-PAGE-IS-FULL                    VALUE '04'.    GBIFPGM 
00312                                                                   GBIFPGM 
00313 /                                                                 GBIFPGM 
00314 ******************************************************************GBIFPGM 
00315 **  ACCUM HOLD AREAS                                            **GBIFPGM 
00316 ******************************************************************GBIFPGM 
00317                                                                   GBIFPGM 
00318  01  WS-LOOP-ACL-HOLD.                                            GBIFPGM 
00319  COPY GCTACLC.                                                    GBIFPGM 
00320                                                                   GBIFPGM 
00321  01  WS-LOOP-ACP-HOLD.                                            GBIFPGM 
00322  COPY GCTACPC.                                                    GBIFPGM 
00323                                                                   GBIFPGM 
00324  01  WS-LOOP-ADL-HOLD.                                            GBIFPGM 
00325  COPY GCTADLC.                                                    GBIFPGM 
00326                                                                   GBIFPGM 
00327  01  WS-LOOP-AOL-HOLD.                                            GBIFPGM 
00328  COPY GCTAOLC.                                                    GBIFPGM 
00329                                                                   GBIFPGM 
00330  01  WS-CON-ABM-HOLD.                                             GBIFPGM 
00331  COPY GCTABM2.                                                    GBIFPGM 
00332                                                                   GBIFPGM 
00333  01  WS-CON-ACL-HOLD.                                             GBIFPGM 
00334  COPY GCTACL2.                                                    GBIFPGM 
00335                                                                   GBIFPGM 
00336  01  WS-CON-ACP-HOLD.                                             GBIFPGM 
00337  COPY GCTACP2.                                                    GBIFPGM 
00338                                                                   GBIFPGM 
00339  01  WS-CON-ADL-HOLD.                                             GBIFPGM 
00340  COPY GCTADL2.                                                    GBIFPGM 
00341                                                                   GBIFPGM 
00342  01  WS-CON-AOL-HOLD.                                             GBIFPGM 
00343  COPY GCTAOL2.                                                    GBIFPGM 
00344                                                                   GBIFPGM 
00345  01  WS-GRP-ABM-HOLD.                                             GBIFPGM 
00346  COPY GCTABM3.                                                    GBIFPGM 
00347                                                                   GBIFPGM 
00348  01  WS-GRP-ACL-HOLD.                                             GBIFPGM 
00349  COPY GCTACL3.                                                    GBIFPGM 
00350                                                                   GBIFPGM 
00351  01  WS-GRP-ACP-HOLD.                                             GBIFPGM 
00352  COPY GCTACP3.                                                    GBIFPGM 
00353                                                                   GBIFPGM 
00354  01  WS-GRP-ADL-HOLD.                                             GBIFPGM 
00355  COPY GCTADL3.                                                    GBIFPGM 
00356                                                                   GBIFPGM 
00357  01  WS-GRP-AOL-HOLD.                                             GBIFPGM 
00358  COPY GCTAOL3.                                                    GBIFPGM 
00359                                                                   GBIFPGM 
00360                                                                   GBIFPGM 
00361 *************** DATE ROUTINE COMMAREA ****************************GBIFPGM 
00362  01  HGADATES-COMMAREA.                                           GBIFPGM 
00363  COPY HGCDAT01.                                                   GBIFPGM 
00364                                                                   GBIFPGM 
00365  01  WS-REC-LENGTHS.                                              GBIFPGM 
00366  COPY GCCDRLEN.                                                   GBIFPGM 
00367                                                                   GBIFPGM 
00368  01  WS-CONSTANTS.                                                GBIFPGM 
00369      05  WS-GCCONTRC-KEYLEN      PIC S9(4) COMP VALUE +36.        GBIFPGM 
00370      05  WS-IO-PARM-CONTRACT-LEN PIC S9(4) COMP VALUE +0.         GBIFPGM 
00371      05  WS-IO-PARM-GROUPSPC-LEN PIC S9(4) COMP VALUE +0.         GBIFPGM 
00372      05  WS-IO-PARM-TABULAR-LEN  PIC S9(4) COMP VALUE +0.         GBIFPGM 
00373      05  WS-GCCOMKEC-LENGTH      PIC S9(4) COMP VALUE +150.       GBIFPGM 
00374 /                                                                 GBIFPGM 
00375 ******************************************************************GBIFPGM 
00376 **  GROUPSPC FILE AND I/O PARM AREA.                            **GBIFPGM 
00377 ******************************************************************GBIFPGM 
00378  01  IO-PARM-GROUPSPC-AREA-1.                                     GBIFPGM 
00379      COPY  GCIOPRM1.                                              GBIFPGM 
00380      COPY  GCGROUPC.                                              GBIFPGM 
00381                                                                   GBIFPGM 
00382 /                                                                 GBIFPGM 
00383 ******************************************************************GBIFPGM 
00384 **  CONTRACT FILE AND I/O PARM AREA.                            **GBIFPGM 
00385 ******************************************************************GBIFPGM 
00386  01  IO-PARM-CONTRACT-AREA-1.                                     GBIFPGM 
00387      COPY  GCIOPRM3.                                              GBIFPGM 
00388      COPY  GCCONTRC.                                              GBIFPGM 
00389                                                                   GBIFPGM 
00390 /                                                                 GBIFPGM 
00391 ******************************************************************GBIFPGM 
00392 **  TABULAR FILE AND I/O PARM AREA.                             **GBIFPGM 
00393 ******************************************************************GBIFPGM 
00394  01  IO-PARM-TABULAR-AREA-1.                                      GBIFPGM 
00395  COPY GCIOPRM5.                                                   GBIFPGM 
00396  COPY GCTABMC.                                                    GBIFPGM 
00397                                                                   GBIFPGM 
00398                                                                   GBIFPGM 
00399 /                                                                 GBIFPGM 
00400 ******************************************************************GBIFPGM 
00401 ** ATTRIBUTE BYTE SETTINGS                                      **GBIFPGM 
00402 ******************************************************************GBIFPGM 
00403  COPY DFHBMSCA.                                                   GBIFPGM 
00404      02  DFHBMABF                PIC X VALUE '9'.                 GBIFPGM 
00405                                                                   GBIFPGM 
00406 /                                                                 GBIFPGM 
00407 ******************************************************************GBIFPGM 
00408 ** ATTENTION IDENTIFIERS                                        **GBIFPGM 
00409 ******************************************************************GBIFPGM 
00410  COPY DFHAID.                                                     GBIFPGM 
00411                                                                   GBIFPGM 
00412 /                                                                 GBIFPGM 
00413 ******************************************************************GBIFPGM 
00414 ** HEX VALUES FOR MILL DATES.                                   **GBIFPGM 
00415 ******************************************************************GBIFPGM 
00416  COPY HEXCOBOL.                                                   GBIFPGM 
00417                                                                   GBIFPGM 
00418  01  WS-END                      PIC X(16)  VALUE                 GBIFPGM 
00419      '*** W/S ENDS ***'.                                          GBIFPGM 
00420 /                                                                 GBIFPGM 
00421  LINKAGE SECTION.                                                 GBIFPGM 
00422                                                                   GBIFPGM 
00423  01  DFHCOMMAREA.                                                 GBIFPGM 
00424  COPY GICOMKE2.                                                   GBIFPGM 
00425  COPY GCBENHLC.                                                   GBIFPGM 
00426                                                                   GBIFPGM 
00427 **- ALSO ADD AREA FOR RECEIVING THE SUBSCRIBER ID ---------------*GBIFPGM 
00428                                                                   GBIFPGM 
00429 /                                                                 GBIFPGM 
00430  PROCEDURE DIVISION.                                              GBIFPGM 
00431 ******************************************************************GBIFPGM 
00432 **                    M A I N L I N E                            *GBIFPGM 
00433 **                                                               *GBIFPGM 
00434 ** THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONS *GBIFPGM 
00435 ** TAKEN BY THE OPERATOR.                                        *GBIFPGM 
00436 **                                                               *GBIFPGM 
00437 ******************************************************************GBIFPGM 
00438  0000-MAINLINE.                                                   GBIFPGM 
00439                                                                   GBIFPGM 
00440      MOVE '0000' TO WS-PARA-ID.                                   GBIFPGM 
00441      MOVE LOW-VALUES TO WS-HEX-00.                                GBIFPGM 
00442                                                                   GBIFPGM 
00443      EXEC CICS HANDLE CONDITION                                   GBIFPGM 
00444           MAPFAIL(0000-SECURITY-VIOLATION)                        GBIFPGM 
00445           END-EXEC.                                               GBIFPGM 
00446                                                                   GBIFPGM 
00447      EXEC CICS                                                    GBIFPGM 
00448           ASSIGN                                                  GBIFPGM 
00449           SYSID(WS-SYSID)                                         GBIFPGM 
00450      END-EXEC                                                     GBIFPGM 
00451 *    IF WS-TEXAS-CICS-REGION                                      GBIFPGM 
00452 *       MOVE 'Y' TO WS-ERROR-FOUND-SW                             GBIFPGM 
00453 *    END-IF                                                       GBIFPGM 
00454                                                                   GBIFPGM 
00455                                                                   GBIFPGM 
00456      PERFORM 1000-PROCESS.                                        GBIFPGM 
00457                                                                   GBIFPGM 
00458 ** NEXT RETURN TO THE CALLING PROGRAM WITH ==> GCBENHLC *******   GBIFPGM 
00459      PERFORM 0099-RETURN.                                         GBIFPGM 
00460                                                                   GBIFPGM 
00461 /                                                                 GBIFPGM 
00462 ******************************************************************GBIFPGM 
00463 ** IF WE HAVE REACHED THIS POINT, THEN THE TERMINAL USER KEYED  **GBIFPGM 
00464 ** THE TRANSID 'GBIF' ON A CLEARED SCREEN OR IN THE CORNER OF   **GBIFPGM 
00465 ** SOME OTHER MAP.  TERMINAL USERS SHOULD INVOKE THIS PROGRAM   **GBIFPGM 
00466 ** ONLY BY GOING THROUGH APPROPRIATE HIGHER-LEVEL PROGRAMS.     **GBIFPGM 
00467 ******************************************************************GBIFPGM 
00468  0000-SECURITY-VIOLATION.                                         GBIFPGM 
00469                                                                   GBIFPGM 
00470      MOVE '0000' TO WS-PARA-ID.                                   GBIFPGM 
00471                                                                   GBIFPGM 
00472      EXEC CICS XCTL                                               GBIFPGM 
00473           PROGRAM('GCS1PGM')                                      GBIFPGM 
00474           END-EXEC.                                               GBIFPGM 
00475                                                                   GBIFPGM 
00476  0000-MAINLINE-EXIT.                                              GBIFPGM 
00477      EXIT.                                                        GBIFPGM 
00478 /                                                                 GBIFPGM 
00479 ******************************************************************GBIFPGM 
00480 *                                                                 GBIFPGM 
00481 ******************************************************************GBIFPGM 
00482  0099-RETURN.                                                     GBIFPGM 
00483                                                                   GBIFPGM 
00484      MOVE '0099' TO WS-PARA-ID.                                   GBIFPGM 
00485                                                                   GBIFPGM 
00486      EXEC CICS                                                    GBIFPGM 
00487           RETURN                                                  GBIFPGM 
00488           END-EXEC.                                               GBIFPGM 
00489                                                                   GBIFPGM 
00490  0099-EXIT.                                                       GBIFPGM 
00491      EXIT.                                                        GBIFPGM 
00492 /                                                                 GBIFPGM 
00493 ******************************************************************GBIFPGM 
00494 *                                                                 GBIFPGM 
00495 *  DISPLAY FIRST SCREEN OF BENEFIT HIGHLIGHTS                     GBIFPGM 
00496 *                                                                 GBIFPGM 
00497 ******************************************************************GBIFPGM 
00498  1000-PROCESS.                                                    GBIFPGM 
00499                                                                   GBIFPGM 
00500      MOVE '1000' TO WS-PARA-ID.                                   GBIFPGM 
00501                                                                   GBIFPGM 
00502      MOVE SPACES                 TO GCBH-BENEFIT-HIGHLIGHTS-REC.  GBIFPGM 
00503                                                                   GBIFPGM 
00504      MOVE GIC2-CONTRACT-ID       TO GCT-CONTRACT-ID.              GBIFPGM 
00505      MOVE GIG2-GROUP-SPECIFIC-ID TO GCG-GRP-SPECIF-ID.            GBIFPGM 
00506                                                                   GBIFPGM 
00507      PERFORM 8000-READ-GROUPSPC.                                  GBIFPGM 
00508      PERFORM 8100-READ-CONTRACT.                                  GBIFPGM 
00509                                                                   GBIFPGM 
00510 *JP 03/30/04                                                      GBIFPGM 
00511      MOVE GCT-PRODUCT-TYPE TO WS-TEST-PRODUCT-TYPE                GBIFPGM 
00512                                                                   GBIFPGM 
00513                                                                   GBIFPGM 
00514      PERFORM 3000-LOOP-THRU-GRP-ACCUMS                            GBIFPGM 
00515         VARYING GCG-INDEX FROM 1 BY 1                             GBIFPGM 
00516            UNTIL GCG-INDEX > 30 OR                                GBIFPGM 
00517                  GCG-TAB-ID (GCG-INDEX) > '#AOL  '.               GBIFPGM 
00518                                                                   GBIFPGM 
00519      PERFORM 3500-LOOP-THRU-CON-ACCUMS                            GBIFPGM 
00520         VARYING GCT-TAB-INDEX FROM 1 BY 1                         GBIFPGM 
00521            UNTIL GCT-TAB-INDEX > 18 OR                            GBIFPGM 
00522                  GCT-CON-TAB-ID (GCT-TAB-INDEX) > '#AOL  '.       GBIFPGM 
00523                                                                   GBIFPGM 
00524 *    EXEC CICS GETMAIN  SET (DFHCOMMAREA)                         GBIFPGM 
00525 *                       INITIMG(WS-HEX-00)                        GBIFPGM 
00526 *                       LENGTH (LENGTH OF DFHCOMMAREA)            GBIFPGM 
00527 *                       END-EXEC.                                 GBIFPGM 
00528                                                                   GBIFPGM 
00529      PERFORM 4000-000-BUILD-RULES THRU 4000-900-EXIT.             GBIFPGM 
00530                                                                   GBIFPGM 
00531 *MQ 05/13/04                                                      GBIFPGM 
00532      IF WS-IL-CICS-REGION                                         GBIFPGM 
00533         PERFORM 6000-000-DEFAULT-VALUES THRU 6000-900-EXIT.       GBIFPGM 
00534                                                                   GBIFPGM 
00535                                                                   GBIFPGM 
00536  1000-EXIT.                                                       GBIFPGM 
00537      EXIT.                                                        GBIFPGM 
00538 /                                                                 GBIFPGM 
00539 ******************************************************************GBIFPGM 
00540 *                                                                 GBIFPGM 
00541 *  LOOP THRU THE ACCUMS LOOKING FOR A SLOT GREATER THAN ZERO.     GBIFPGM 
00542 *  IF FOUND, PROCESS THE ACCUM.                                   GBIFPGM 
00543 *                                                                 GBIFPGM 
00544 ******************************************************************GBIFPGM 
00545  3000-LOOP-THRU-GRP-ACCUMS.                                       GBIFPGM 
00546                                                                   GBIFPGM 
00547      IF GCG-TAB-ID (GCG-INDEX) = '#ABM  ' OR '#ACL  ' OR          GBIFPGM 
00548                                  '#ACP  ' OR '#ADL  ' OR '#AOL  ' GBIFPGM 
00549         IF GCG-TAB-SLOT-NO (GCG-INDEX) > ZERO                     GBIFPGM 
00550            PERFORM 3100-PROCESS-GRP-ACCUM.                        GBIFPGM 
00551                                                                   GBIFPGM 
00552  3000-EXIT.                                                       GBIFPGM 
00553      EXIT.                                                        GBIFPGM 
00554 /                                                                 GBIFPGM 
00555 ******************************************************************GBIFPGM 
00556 *                                                                 GBIFPGM 
00557 *  FOR EACH ACCUM FOUND, MOVE IT TO THE GROUP SPECIFIC HOLD ACCUM GBIFPGM 
00558 *  AND SET FOUND SWITCH.                                          GBIFPGM 
00559 *                                                                 GBIFPGM 
00560 ******************************************************************GBIFPGM 
00561  3100-PROCESS-GRP-ACCUM.                                          GBIFPGM 
00562                                                                   GBIFPGM 
00563      MOVE GCG-GRP-SPEC-TAB-ID (GCG-INDEX)                         GBIFPGM 
00564        TO GAA-TABULAR-PROVISION-ID.                               GBIFPGM 
00565                                                                   GBIFPGM 
00566      PERFORM 8200-READ-TABULAR.                                   GBIFPGM 
00567                                                                   GBIFPGM 
00568      IF GAA-PROVISION-ID = '#ABM  '                               GBIFPGM 
00569         MOVE GAA-RECORD  TO WS-GRP-ABM-HOLD                       GBIFPGM 
00570         MOVE 'Y'         TO WS-GRP-ABM-FOUND-SW                   GBIFPGM 
00571      ELSE                                                         GBIFPGM 
00572      IF GAA-PROVISION-ID = '#ACL  '                               GBIFPGM 
00573         MOVE GAA-RECORD  TO WS-GRP-ACL-HOLD                       GBIFPGM 
00574         MOVE 'Y'         TO WS-GRP-ACL-FOUND-SW                   GBIFPGM 
00575      ELSE                                                         GBIFPGM 
00576      IF GAA-PROVISION-ID = '#ACP  '                               GBIFPGM 
00577         MOVE GAA-RECORD  TO WS-GRP-ACP-HOLD                       GBIFPGM 
00578         MOVE 'Y'         TO WS-GRP-ACP-FOUND-SW                   GBIFPGM 
00579      ELSE                                                         GBIFPGM 
00580      IF GAA-PROVISION-ID = '#ADL  '                               GBIFPGM 
00581         MOVE GAA-RECORD  TO WS-GRP-ADL-HOLD                       GBIFPGM 
00582         MOVE 'Y'         TO WS-GRP-ADL-FOUND-SW                   GBIFPGM 
00583      ELSE                                                         GBIFPGM 
00584      IF GAA-PROVISION-ID = '#AOL  '                               GBIFPGM 
00585         MOVE GAA-RECORD  TO WS-GRP-AOL-HOLD                       GBIFPGM 
00586         MOVE 'Y'         TO WS-GRP-AOL-FOUND-SW.                  GBIFPGM 
00587                                                                   GBIFPGM 
00588  3100-EXIT.                                                       GBIFPGM 
00589      EXIT.                                                        GBIFPGM 
00590 /                                                                 GBIFPGM 
00591 ******************************************************************GBIFPGM 
00592 *                                                                 GBIFPGM 
00593 *  LOOP THRU THE ACCUMS LOOKING FOR A SLOT GREATER THAN ZERO.     GBIFPGM 
00594 *  IF FOUND, PROCESS THE ACCUM.                                   GBIFPGM 
00595 *                                                                 GBIFPGM 
00596 ******************************************************************GBIFPGM 
00597  3500-LOOP-THRU-CON-ACCUMS.                                       GBIFPGM 
00598                                                                   GBIFPGM 
00599      IF GCT-CON-TAB-ID (GCT-TAB-INDEX) = '#ABM  ' OR '#ACL  ' OR  GBIFPGM 
00600                              '#ACP  ' OR '#ADL  ' OR '#AOL  '     GBIFPGM 
00601         IF GCT-CON-TAB-SLOT (GCT-TAB-INDEX) > ZERO                GBIFPGM 
00602            PERFORM 3600-PROCESS-CON-ACCUM.                        GBIFPGM 
00603                                                                   GBIFPGM 
00604  3500-EXIT.                                                       GBIFPGM 
00605      EXIT.                                                        GBIFPGM 
00606 /                                                                 GBIFPGM 
00607 ******************************************************************GBIFPGM 
00608 *                                                                 GBIFPGM 
00609 *                                                                 GBIFPGM 
00610 *  FOR EACH ACCUM FOUND, MOVE IT TO THE CONTRACT HOLD ACCUM       GBIFPGM 
00611 *  AND SET FOUND SWITCH.                                          GBIFPGM 
00612 *                                                                 GBIFPGM 
00613 *                                                                 GBIFPGM 
00614 ******************************************************************GBIFPGM 
00615  3600-PROCESS-CON-ACCUM.                                          GBIFPGM 
00616                                                                   GBIFPGM 
00617      MOVE GCT-CON-TAB-ID-SLOT (GCT-TAB-INDEX)                     GBIFPGM 
00618        TO GAA-TABULAR-PROVISION-ID.                               GBIFPGM 
00619                                                                   GBIFPGM 
00620      PERFORM 8200-READ-TABULAR.                                   GBIFPGM 
00621                                                                   GBIFPGM 
00622      IF GAA-PROVISION-ID = '#ABM  '                               GBIFPGM 
00623         MOVE GAA-RECORD  TO WS-CON-ABM-HOLD                       GBIFPGM 
00624         MOVE 'Y'         TO WS-CON-ABM-FOUND-SW                   GBIFPGM 
00625      ELSE                                                         GBIFPGM 
00626      IF GAA-PROVISION-ID = '#ACL  '                               GBIFPGM 
00627         MOVE GAA-RECORD  TO WS-CON-ACL-HOLD                       GBIFPGM 
00628         MOVE 'Y'         TO WS-CON-ACL-FOUND-SW                   GBIFPGM 
00629      ELSE                                                         GBIFPGM 
00630      IF GAA-PROVISION-ID = '#ACP  '                               GBIFPGM 
00631         MOVE GAA-RECORD  TO WS-CON-ACP-HOLD                       GBIFPGM 
00632         MOVE 'Y'         TO WS-CON-ACP-FOUND-SW                   GBIFPGM 
00633      ELSE                                                         GBIFPGM 
00634      IF GAA-PROVISION-ID = '#ADL  '                               GBIFPGM 
00635         MOVE GAA-RECORD  TO WS-CON-ADL-HOLD                       GBIFPGM 
00636         MOVE 'Y'         TO WS-CON-ADL-FOUND-SW                   GBIFPGM 
00637      ELSE                                                         GBIFPGM 
00638      IF GAA-PROVISION-ID = '#AOL  '                               GBIFPGM 
00639         MOVE GAA-RECORD  TO WS-CON-AOL-HOLD                       GBIFPGM 
00640         MOVE 'Y'         TO WS-CON-AOL-FOUND-SW.                  GBIFPGM 
00641                                                                   GBIFPGM 
00642  3600-EXIT.                                                       GBIFPGM 
00643      EXIT.                                                        GBIFPGM 
00644 /                                                                 GBIFPGM 
00645 ******************************************************************GBIFPGM 
00646 *                                                                 GBIFPGM 
00647 *  BUILD RULE LINES UNTIL PAGE IS FULL OR END OF RULES.           GBIFPGM 
00648 *  WHEN A PAGE IS FULL, WRITE IT TO TEMPORARY STORAGE AND THEN    GBIFPGM 
00649 *  CONTINUE WITH THE NEXT PAGE.                                   GBIFPGM 
00650 *                                                                 GBIFPGM 
00651 ******************************************************************GBIFPGM 
00652  4000-000-BUILD-RULES        SECTION.                             GBIFPGM 
00653  4000-4020.                                                       GBIFPGM 
00654                                                                   GBIFPGM 
00655      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00656         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00657         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00658         PERFORM 4020-BLD-DED-PER-IND                              GBIFPGM 
00659           VARYING GAC-INDEX                                       GBIFPGM 
00660             FROM 1 BY 1                                           GBIFPGM 
00661               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00662      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00663         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00664         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00665         PERFORM 4020-BLD-DED-PER-IND                              GBIFPGM 
00666           VARYING GAC-INDEX                                       GBIFPGM 
00667             FROM 1 BY 1                                           GBIFPGM 
00668               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00669                                                                   GBIFPGM 
00733  4000-4022.                                                       GBIFPGM 
00734                                                                   GBIFPGM 
00735      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00736         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00737         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00738         PERFORM 4022-BLD-IND-ALONE-DED                            GBIFPGM 
00739           VARYING GAC-INDEX                                       GBIFPGM 
00740             FROM 1 BY 1                                           GBIFPGM 
00741               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00742      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00743         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00744         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00745         PERFORM 4022-BLD-IND-ALONE-DED                            GBIFPGM 
00746           VARYING GAC-INDEX                                       GBIFPGM 
00747             FROM 1 BY 1                                           GBIFPGM 
00748               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00749                                                                   GBIFPGM 
00750  4000-4025.                                                       GBIFPGM 
00751                                                                   GBIFPGM 
00752      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00753         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00754         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00755         PERFORM 4025-BLD-COMB-IND-DED                             GBIFPGM 
00756           VARYING GAC-INDEX                                       GBIFPGM 
00757             FROM 1 BY 1                                           GBIFPGM 
00758               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00759      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00760         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00761         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00762         PERFORM 4025-BLD-COMB-IND-DED                             GBIFPGM 
00763           VARYING GAC-INDEX                                       GBIFPGM 
00764             FROM 1 BY 1                                           GBIFPGM 
00765               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00766                                                                   GBIFPGM 
00670  4000-4030.                                                       GBIFPGM 
00671                                                                   GBIFPGM 
00672      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00673         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00674         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00675         PERFORM 4030-BLD-DED-PER-FAM                              GBIFPGM 
00676           VARYING GAC-INDEX                                       GBIFPGM 
00677             FROM 1 BY 1                                           GBIFPGM 
00678               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00679      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00680         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00681         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00682         PERFORM 4030-BLD-DED-PER-FAM                              GBIFPGM 
00683           VARYING GAC-INDEX                                       GBIFPGM 
00684             FROM 1 BY 1                                           GBIFPGM 
00685               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00686                                                                   GBIFPGM 
00784  4000-4032.                                                       GBIFPGM 
00785                                                                   GBIFPGM 
00786      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00787         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00788         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00789         PERFORM 4032-BLD-FAM-ALONE-DED                            GBIFPGM 
00790           VARYING GAC-INDEX                                       GBIFPGM 
00791             FROM 1 BY 1                                           GBIFPGM 
00792               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00793      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00794         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00795         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00796         PERFORM 4032-BLD-FAM-ALONE-DED                            GBIFPGM 
00797           VARYING GAC-INDEX                                       GBIFPGM 
00798             FROM 1 BY 1                                           GBIFPGM 
00799               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00800                                                                   GBIFPGM 
00801  4000-4035.                                                       GBIFPGM 
00802                                                                   GBIFPGM 
00803      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00804         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00805         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00806         PERFORM 4035-BLD-COMB-FAM-DED                             GBIFPGM 
00807           VARYING GAC-INDEX                                       GBIFPGM 
00808             FROM 1 BY 1                                           GBIFPGM 
00809               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00810      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00811         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00812         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00813         PERFORM 4035-BLD-COMB-FAM-DED                             GBIFPGM 
00814           VARYING GAC-INDEX                                       GBIFPGM 
00815             FROM 1 BY 1                                           GBIFPGM 
00816               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00817                                                                   GBIFPGM 
00687  4000-4040.                                                       GBIFPGM 
00688                                                                   GBIFPGM 
00689      IF WS-GRP-AOL-FOUND                                          GBIFPGM 
00690         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00691         MOVE WS-GRP-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00692         PERFORM 4040-BLD-OOP-PER-IND                              GBIFPGM 
00693           VARYING GAD-INDEX                                       GBIFPGM 
00694             FROM 1 BY 1                                           GBIFPGM 
00695               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00696      IF WS-CON-AOL-FOUND                                          GBIFPGM 
00697         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00698         MOVE WS-CON-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00699         PERFORM 4040-BLD-OOP-PER-IND                              GBIFPGM 
00700           VARYING GAD-INDEX                                       GBIFPGM 
00701             FROM 1 BY 1                                           GBIFPGM 
00702               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00703                                                                   GBIFPGM 
00835                                                                   GBIFPGM 
00836  4000-4042.                                                       GBIFPGM 
00837                                                                   GBIFPGM 
00838      IF WS-GRP-AOL-FOUND                                          GBIFPGM 
00839         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00840         MOVE WS-GRP-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00841         PERFORM 4042-BLD-IND-ALONE-OOP                            GBIFPGM 
00842           VARYING GAD-INDEX                                       GBIFPGM 
00843             FROM 1 BY 1                                           GBIFPGM 
00844               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00845      IF WS-CON-AOL-FOUND                                          GBIFPGM 
00846         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00847         MOVE WS-CON-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00848         PERFORM 4042-BLD-IND-ALONE-OOP                            GBIFPGM 
00849           VARYING GAD-INDEX                                       GBIFPGM 
00850             FROM 1 BY 1                                           GBIFPGM 
00851               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00852                                                                   GBIFPGM 
00853  4000-4045.                                                       GBIFPGM 
00854                                                                   GBIFPGM 
00855      IF WS-GRP-AOL-FOUND                                          GBIFPGM 
00856         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00857         MOVE WS-GRP-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00858         PERFORM 4045-BLD-COMB-IND-OOP                             GBIFPGM 
00859           VARYING GAD-INDEX                                       GBIFPGM 
00860             FROM 1 BY 1                                           GBIFPGM 
00861               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00862      IF WS-CON-AOL-FOUND                                          GBIFPGM 
00863         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00864         MOVE WS-CON-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00865         PERFORM 4045-BLD-COMB-IND-OOP                             GBIFPGM 
00866           VARYING GAD-INDEX                                       GBIFPGM 
00867             FROM 1 BY 1                                           GBIFPGM 
00868               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00869                                                                   GBIFPGM 
00704  4000-4050.                                                       GBIFPGM 
00705                                                                   GBIFPGM 
00706      IF WS-GRP-AOL-FOUND                                          GBIFPGM 
00707         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00708         MOVE WS-GRP-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00709         PERFORM 4050-BLD-OOP-PER-FAM                              GBIFPGM 
00710           VARYING GAD-INDEX                                       GBIFPGM 
00711             FROM 1 BY 1                                           GBIFPGM 
00712               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00713      IF WS-CON-AOL-FOUND                                          GBIFPGM 
00714         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00715         MOVE WS-CON-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00716         PERFORM 4050-BLD-OOP-PER-FAM                              GBIFPGM 
00883           VARYING GAD-INDEX                                       GBIFPGM 
00884             FROM 1 BY 1                                           GBIFPGM 
00885               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00886                                                                   GBIFPGM 
00887                                                                   GBIFPGM 
00888  4000-4052.                                                       GBIFPGM 
00889                                                                   GBIFPGM 
00890      IF WS-GRP-AOL-FOUND                                          GBIFPGM 
00891         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00892         MOVE WS-GRP-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00893         PERFORM 4052-BLD-FAM-ALONE-OOP                            GBIFPGM 
00894           VARYING GAD-INDEX                                       GBIFPGM 
00895             FROM 1 BY 1                                           GBIFPGM 
00896               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00897      IF WS-CON-AOL-FOUND                                          GBIFPGM 
00898         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00899         MOVE WS-CON-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00900         PERFORM 4052-BLD-FAM-ALONE-OOP                            GBIFPGM 
00901           VARYING GAD-INDEX                                       GBIFPGM 
00902             FROM 1 BY 1                                           GBIFPGM 
00903               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00904                                                                   GBIFPGM 
00905                                                                   GBIFPGM 
00906  4000-4055.                                                       GBIFPGM 
00907                                                                   GBIFPGM 
00908      IF WS-GRP-AOL-FOUND                                          GBIFPGM 
00909         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00910         MOVE WS-GRP-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00911         PERFORM 4055-BLD-COMB-FAM-OOP                             GBIFPGM 
00912           VARYING GAD-INDEX                                       GBIFPGM 
00913             FROM 1 BY 1                                           GBIFPGM 
00914               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00915      IF WS-CON-AOL-FOUND                                          GBIFPGM 
00916         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00917         MOVE WS-CON-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
00918         PERFORM 4055-BLD-COMB-FAM-OOP                             GBIFPGM 
00717           VARYING GAD-INDEX                                       GBIFPGM 
00718             FROM 1 BY 1                                           GBIFPGM 
00719               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
00720                                                                   GBIFPGM 
00721  4000-4060.                                                       GBIFPGM 
00722                                                                   GBIFPGM 
00723      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
00724         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00725         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
00726         PERFORM 4060-BLD-LIFETIME-MAX                             GBIFPGM 
00727           VARYING GAA-INDEX                                       GBIFPGM 
00728             FROM 1 BY 1                                           GBIFPGM 
00729               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
00730      IF WS-CON-ABM-FOUND                                          GBIFPGM 
00731         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00732         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
00733         PERFORM 4060-BLD-LIFETIME-MAX                             GBIFPGM 
00734           VARYING GAA-INDEX                                       GBIFPGM 
00735             FROM 1 BY 1                                           GBIFPGM 
00736               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
00737                                                                   GBIFPGM 
00738  4000-4070.                                                       GBIFPGM 
00739                                                                   GBIFPGM 
00740 *MQ 6/06/03                                                       GBIFPGM 
00741                                                                   GBIFPGM 
00742      MOVE 'N' TO WS-ERCI-VALQUAL5-SW                              GBIFPGM 
00743                  WS-ERCO-VALQUAL5-SW                              GBIFPGM 
00744                  WS-ERCA-VALQUAL5-SW                              GBIFPGM 
00745                                                                   GBIFPGM 
00746      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
00747         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00748         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00749         PERFORM 4070-BLD-EMER-RM-COPAY                            GBIFPGM 
00750           VARYING GAF-INDEX                                       GBIFPGM 
00751             FROM 1 BY 1                                           GBIFPGM 
00752               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00753      IF WS-CON-ACP-FOUND                                          GBIFPGM 
00754         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00755         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00756         PERFORM 4070-BLD-EMER-RM-COPAY                            GBIFPGM 
00757           VARYING GAF-INDEX                                       GBIFPGM 
00758             FROM 1 BY 1                                           GBIFPGM 
00759               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00760                                                                   GBIFPGM 
00761      IF ERCI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00762         ERCO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00763         ERCA-VALQUAL5-SW-OFF                                      GBIFPGM 
00764         PERFORM 4000-4070A.                                       GBIFPGM 
00765                                                                   GBIFPGM 
00766  4000-4070A.                                                      GBIFPGM 
00767                                                                   GBIFPGM 
00768 *MQ 6/06/03                                                       GBIFPGM 
00769                                                                   GBIFPGM 
00770      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00771         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00772         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00773         PERFORM 4070A-BLD-EMER-RM-COPAY                           GBIFPGM 
00774           VARYING GAC-INDEX                                       GBIFPGM 
00775             FROM 1 BY 1                                           GBIFPGM 
00776               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00777      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00778         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00779         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00780         PERFORM 4070A-BLD-EMER-RM-COPAY                           GBIFPGM 
00781           VARYING GAC-INDEX                                       GBIFPGM 
00782             FROM 1 BY 1                                           GBIFPGM 
00783               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00986                                                                   GBIFPGM 
00987                                                                   GBIFPGM 
00988  4000-4071.                                                       GBIFPGM 
00989                                                                   GBIFPGM 
00990 *MQ 5/11/05                                                       GBIFPGM 
00991                                                                   GBIFPGM 
00992      MOVE 'N' TO WS-ACOI-VALQUAL5-SW                              GBIFPGM 
00993                  WS-ACOO-VALQUAL5-SW                              GBIFPGM 
00994                  WS-ACOA-VALQUAL5-SW                              GBIFPGM 
00995                                                                   GBIFPGM 
00996      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
00997         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00998         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00999         PERFORM 4071-BLD-AMB-COPAY                                GBIFPGM 
01000           VARYING GAF-INDEX                                       GBIFPGM 
01001             FROM 1 BY 1                                           GBIFPGM 
01002               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01003      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01004         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01005         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01006         PERFORM 4071-BLD-AMB-COPAY                                GBIFPGM 
01007           VARYING GAF-INDEX                                       GBIFPGM 
01008             FROM 1 BY 1                                           GBIFPGM 
01009               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01010                                                                   GBIFPGM 
01011      IF ACOI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01012         ACOO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01013         ACOA-VALQUAL5-SW-OFF                                      GBIFPGM 
01014         PERFORM 4000-4071A.                                       GBIFPGM 
01015                                                                   GBIFPGM 
01016  4000-4071A.                                                      GBIFPGM 
01017                                                                   GBIFPGM 
01018 *MQ 5/11/05                                                       GBIFPGM 
01019                                                                   GBIFPGM 
01020      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01021         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01022         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01023         PERFORM 4071A-BLD-AMB-COPAY                               GBIFPGM 
01024           VARYING GAC-INDEX                                       GBIFPGM 
01025             FROM 1 BY 1                                           GBIFPGM 
01026               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01027      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01028         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01029         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01030         PERFORM 4071A-BLD-AMB-COPAY                               GBIFPGM 
01031           VARYING GAC-INDEX                                       GBIFPGM 
01032             FROM 1 BY 1                                           GBIFPGM 
01033               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01034                                                                   GBIFPGM 
01035                                                                   GBIFPGM 
01036  4000-4072.                                                       GBIFPGM 
01037                                                                   GBIFPGM 
01038 *MQ 5/11/05                                                       GBIFPGM 
01039                                                                   GBIFPGM 
01040      MOVE 'N' TO WS-SNFI-VALQUAL5-SW                              GBIFPGM 
01041                  WS-SNFO-VALQUAL5-SW                              GBIFPGM 
01042                  WS-SNFA-VALQUAL5-SW                              GBIFPGM 
01043                                                                   GBIFPGM 
01044      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
01045         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01046         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01047         PERFORM 4072-BLD-SKILL-NUR-FAC-COPAY                      GBIFPGM 
01048           VARYING GAF-INDEX                                       GBIFPGM 
01049             FROM 1 BY 1                                           GBIFPGM 
01050               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01051      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01052         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01053         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01054         PERFORM 4072-BLD-SKILL-NUR-FAC-COPAY                      GBIFPGM 
01055           VARYING GAF-INDEX                                       GBIFPGM 
01056             FROM 1 BY 1                                           GBIFPGM 
01057               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01058                                                                   GBIFPGM 
01059      IF SNFI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01060         SNFO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01061         SNFA-VALQUAL5-SW-OFF                                      GBIFPGM 
01062         PERFORM 4000-4072A.                                       GBIFPGM 
01063                                                                   GBIFPGM 
01064  4000-4072A.                                                      GBIFPGM 
01065                                                                   GBIFPGM 
01066 *MQ 5/11/05                                                       GBIFPGM 
01067                                                                   GBIFPGM 
01068      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01069         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01070         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01071         PERFORM 4072A-BLD-SKILL-NUR-FAC-COPAY                     GBIFPGM 
01072           VARYING GAC-INDEX                                       GBIFPGM 
01073             FROM 1 BY 1                                           GBIFPGM 
01074               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01075      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01076         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01077         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01078         PERFORM 4072A-BLD-SKILL-NUR-FAC-COPAY                     GBIFPGM 
01079           VARYING GAC-INDEX                                       GBIFPGM 
01080             FROM 1 BY 1                                           GBIFPGM 
01081               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01082                                                                   GBIFPGM 
01083                                                                   GBIFPGM 
01084  4000-4073.                                                       GBIFPGM 
01085                                                                   GBIFPGM 
01086 *MQ 5/11/05                                                       GBIFPGM 
01087                                                                   GBIFPGM 
01088      MOVE 'N' TO WS-MHCI-VALQUAL5-SW                              GBIFPGM 
01089                  WS-MHCO-VALQUAL5-SW                              GBIFPGM 
01090                  WS-MHCA-VALQUAL5-SW                              GBIFPGM 
01091                                                                   GBIFPGM 
01092      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
01093         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01094         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01095         PERFORM 4073-BLD-OP-PSYC-VISIT-COPAY                      GBIFPGM 
01096           VARYING GAF-INDEX                                       GBIFPGM 
01097             FROM 1 BY 1                                           GBIFPGM 
01098               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01099      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01100         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01101         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01102         PERFORM 4073-BLD-OP-PSYC-VISIT-COPAY                      GBIFPGM 
01103           VARYING GAF-INDEX                                       GBIFPGM 
01104             FROM 1 BY 1                                           GBIFPGM 
01105               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01106                                                                   GBIFPGM 
01107      IF MHCI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01108         MHCO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01109         MHCA-VALQUAL5-SW-OFF                                      GBIFPGM 
01110         PERFORM 4000-4073A.                                       GBIFPGM 
01111                                                                   GBIFPGM 
01112  4000-4073A.                                                      GBIFPGM 
01113                                                                   GBIFPGM 
01114 *MQ 5/11/05                                                       GBIFPGM 
01115                                                                   GBIFPGM 
01116      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01117         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01118         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01119         PERFORM 4073A-BLD-OP-PSYC-VISIT-COPAY                     GBIFPGM 
01120           VARYING GAC-INDEX                                       GBIFPGM 
01121             FROM 1 BY 1                                           GBIFPGM 
01122               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01123      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01124         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01125         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01126         PERFORM 4073A-BLD-OP-PSYC-VISIT-COPAY                     GBIFPGM 
01127           VARYING GAC-INDEX                                       GBIFPGM 
01128             FROM 1 BY 1                                           GBIFPGM 
01129               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01130                                                                   GBIFPGM 
01131                                                                   GBIFPGM 
01132  4000-4074.                                                       GBIFPGM 
01133                                                                   GBIFPGM 
01134 *MQ 5/11/05                                                       GBIFPGM 
01135                                                                   GBIFPGM 
01136      MOVE 'N' TO WS-PACI-VALQUAL5-SW                              GBIFPGM 
01137                  WS-PACO-VALQUAL5-SW                              GBIFPGM 
01138                  WS-PACA-VALQUAL5-SW                              GBIFPGM 
01139                                                                   GBIFPGM 
01140      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
01141         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01142         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01143         PERFORM 4074-BLD-IP-PER-ADM-COPAY                         GBIFPGM 
01144           VARYING GAF-INDEX                                       GBIFPGM 
01145             FROM 1 BY 1                                           GBIFPGM 
01146               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01147      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01148         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01149         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01150         PERFORM 4074-BLD-IP-PER-ADM-COPAY                         GBIFPGM 
01151           VARYING GAF-INDEX                                       GBIFPGM 
01152             FROM 1 BY 1                                           GBIFPGM 
01153               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01154                                                                   GBIFPGM 
01155      IF PACI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01156         PACO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01157         PACA-VALQUAL5-SW-OFF                                      GBIFPGM 
01158         PERFORM 4000-4074A.                                       GBIFPGM 
01159                                                                   GBIFPGM 
01160  4000-4074A.                                                      GBIFPGM 
01161                                                                   GBIFPGM 
01162 *MQ 5/11/05                                                       GBIFPGM 
01163                                                                   GBIFPGM 
01164      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01165         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01166         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01167         PERFORM 4074A-BLD-IP-PER-ADM-COPAY                        GBIFPGM 
01168           VARYING GAC-INDEX                                       GBIFPGM 
01169             FROM 1 BY 1                                           GBIFPGM 
01170               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01171      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01172         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01173         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01174         PERFORM 4074A-BLD-IP-PER-ADM-COPAY                        GBIFPGM 
01175           VARYING GAC-INDEX                                       GBIFPGM 
01176             FROM 1 BY 1                                           GBIFPGM 
01177               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01178                                                                   GBIFPGM 
01179                                                                   GBIFPGM 
01180  4000-4075.                                                       GBIFPGM 
01181                                                                   GBIFPGM 
01182 *MQ 5/11/05                                                       GBIFPGM 
01183                                                                   GBIFPGM 
01184      MOVE 'N' TO WS-HCPI-VALQUAL5-SW                              GBIFPGM 
01185                  WS-HCPO-VALQUAL5-SW                              GBIFPGM 
01186                  WS-HCPA-VALQUAL5-SW                              GBIFPGM 
01187                                                                   GBIFPGM 
01188      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
01189         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01190         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01191         PERFORM 4075-BLD-HOME-HLTH-VST-COPAY                      GBIFPGM 
01192           VARYING GAF-INDEX                                       GBIFPGM 
01193             FROM 1 BY 1                                           GBIFPGM 
01194               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01195      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01196         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01197         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01198         PERFORM 4075-BLD-HOME-HLTH-VST-COPAY                      GBIFPGM 
01199           VARYING GAF-INDEX                                       GBIFPGM 
01200             FROM 1 BY 1                                           GBIFPGM 
01201               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01202                                                                   GBIFPGM 
01203      IF HCPI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01204         HCPO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01205         HCPA-VALQUAL5-SW-OFF                                      GBIFPGM 
01206         PERFORM 4000-4075A.                                       GBIFPGM 
01207                                                                   GBIFPGM 
01208  4000-4075A.                                                      GBIFPGM 
01209                                                                   GBIFPGM 
01210 *MQ 5/11/05                                                       GBIFPGM 
01211                                                                   GBIFPGM 
01212      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01213         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01214         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01215         PERFORM 4075A-BLD-HOME-HLTH-VST-COPAY                     GBIFPGM 
01216           VARYING GAC-INDEX                                       GBIFPGM 
01217             FROM 1 BY 1                                           GBIFPGM 
01218               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01219      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01220         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01221         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01222         PERFORM 4075A-BLD-HOME-HLTH-VST-COPAY                     GBIFPGM 
01223           VARYING GAC-INDEX                                       GBIFPGM 
01224             FROM 1 BY 1                                           GBIFPGM 
01225               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01226                                                                   GBIFPGM 
01227  4000-4076.                                                       GBIFPGM 
01228                                                                   GBIFPGM 
01229      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01230         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01231         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01232         PERFORM 4076-BLD-ALLERGY-TST-PMT-LVL                      GBIFPGM 
01233           VARYING GAB-INDEX                                       GBIFPGM 
01234             FROM 1 BY 1                                           GBIFPGM 
01235               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01236      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01237         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01238         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01239         PERFORM 4076-BLD-ALLERGY-TST-PMT-LVL                      GBIFPGM 
01240           VARYING GAB-INDEX                                       GBIFPGM 
01241             FROM 1 BY 1                                           GBIFPGM 
01242               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
00784                                                                   GBIFPGM 
00785  4000-4080.                                                       GBIFPGM 
00786                                                                   GBIFPGM 
00787 *MQ 6/10/03                                                       GBIFPGM 
00788      MOVE 'N' TO WS-OVCI-VALQUAL5-SW                              GBIFPGM 
00789                  WS-OVCO-VALQUAL5-SW                              GBIFPGM 
00790                  WS-OVCA-VALQUAL5-SW                              GBIFPGM 
00791                                                                   GBIFPGM 
00792      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
00793         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00794         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00795         PERFORM 4080-BLD-OFF-VISIT-COPAY                          GBIFPGM 
00796           VARYING GAF-INDEX                                       GBIFPGM 
00797             FROM 1 BY 1                                           GBIFPGM 
00798               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00799      IF WS-CON-ACP-FOUND                                          GBIFPGM 
00800         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00801         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00802         PERFORM 4080-BLD-OFF-VISIT-COPAY                          GBIFPGM 
00803           VARYING GAF-INDEX                                       GBIFPGM 
00804             FROM 1 BY 1                                           GBIFPGM 
00805               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00806                                                                   GBIFPGM 
00807      IF OVCI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00808         OVCO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00809         OVCA-VALQUAL5-SW-OFF                                      GBIFPGM 
00810         PERFORM 4000-4080A.                                       GBIFPGM 
00811                                                                   GBIFPGM 
00812  4000-4080A.                                                      GBIFPGM 
00813                                                                   GBIFPGM 
00814 *MQ 6/10/03                                                       GBIFPGM 
00815      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00816         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00817         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00818         PERFORM 4080A-BLD-OFF-VISIT-COPAY                         GBIFPGM 
00819           VARYING GAC-INDEX                                       GBIFPGM 
00820             FROM 1 BY 1                                           GBIFPGM 
00821               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00822      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00823         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00824         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00825         PERFORM 4080A-BLD-OFF-VISIT-COPAY                         GBIFPGM 
00826           VARYING GAC-INDEX                                       GBIFPGM 
00827             FROM 1 BY 1                                           GBIFPGM 
00828               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00829                                                                   GBIFPGM 
00830  4000-4085.                                                       GBIFPGM 
00831                                                                   GBIFPGM 
00832      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
00833         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00834         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
00835         PERFORM 4085-BLD-OFF-VISIT-PMT-LVL                        GBIFPGM 
00836           VARYING GAB-INDEX                                       GBIFPGM 
00837             FROM 1 BY 1                                           GBIFPGM 
00838               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
00839      IF WS-CON-ACL-FOUND                                          GBIFPGM 
00840         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00841         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
00842         PERFORM 4085-BLD-OFF-VISIT-PMT-LVL                        GBIFPGM 
00843           VARYING GAB-INDEX                                       GBIFPGM 
00844             FROM 1 BY 1                                           GBIFPGM 
00845               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
00846                                                                   GBIFPGM 
00847 *MQ 10/09/03                                                      GBIFPGM 
00848  4000-4086.                                                       GBIFPGM 
00849                                                                   GBIFPGM 
00850      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
00851         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00852         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
00853         PERFORM 4086-BLD-OFF-SURG-PMT-LVL                         GBIFPGM 
00854           VARYING GAB-INDEX                                       GBIFPGM 
00855             FROM 1 BY 1                                           GBIFPGM 
00856               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
00857      IF WS-CON-ACL-FOUND                                          GBIFPGM 
00858         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00859         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
00860         PERFORM 4086-BLD-OFF-SURG-PMT-LVL                         GBIFPGM 
00861           VARYING GAB-INDEX                                       GBIFPGM 
00862             FROM 1 BY 1                                           GBIFPGM 
00863               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
00864                                                                   GBIFPGM 
00865                                                                   GBIFPGM 
00866  4000-4090.                                                       GBIFPGM 
00867                                                                   GBIFPGM 
00868 *MQ 6/10/03                                                       GBIFPGM 
00869      MOVE 'N' TO WS-WCCI-VALQUAL5-SW                              GBIFPGM 
00870                  WS-WCCO-VALQUAL5-SW                              GBIFPGM 
00871                  WS-WCCA-VALQUAL5-SW                              GBIFPGM 
00872                                                                   GBIFPGM 
00873      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
00874         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00875         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00876         PERFORM 4090-BLD-WELL-CARE-COPAY                          GBIFPGM 
00877           VARYING GAF-INDEX                                       GBIFPGM 
00878             FROM 1 BY 1                                           GBIFPGM 
00879               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00880      IF WS-CON-ACP-FOUND                                          GBIFPGM 
00881         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00882         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00883         PERFORM 4090-BLD-WELL-CARE-COPAY                          GBIFPGM 
00884           VARYING GAF-INDEX                                       GBIFPGM 
00885             FROM 1 BY 1                                           GBIFPGM 
00886               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00887                                                                   GBIFPGM 
00888      IF WCCI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00889         WCCO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00890         WCCA-VALQUAL5-SW-OFF                                      GBIFPGM 
00891         PERFORM 4000-4090A.                                       GBIFPGM 
00892                                                                   GBIFPGM 
00893  4000-4090A.                                                      GBIFPGM 
00894                                                                   GBIFPGM 
00895 *MQ 6/10/03                                                       GBIFPGM 
00896      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00897         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00898         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00899         PERFORM 4090A-BLD-WELL-CARE-COPAY                         GBIFPGM 
00900           VARYING GAC-INDEX                                       GBIFPGM 
00901             FROM 1 BY 1                                           GBIFPGM 
00902               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00903      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00904         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00905         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00906         PERFORM 4090A-BLD-WELL-CARE-COPAY                         GBIFPGM 
00907           VARYING GAC-INDEX                                       GBIFPGM 
00908             FROM 1 BY 1                                           GBIFPGM 
00909               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00910                                                                   GBIFPGM 
00911 *MQ 10/09/03                                                      GBIFPGM 
00912  4000-4091.                                                       GBIFPGM 
00913                                                                   GBIFPGM 
00914      MOVE 'N' TO WS-SOVI-VALQUAL5-SW                              GBIFPGM 
00915                  WS-SOVO-VALQUAL5-SW                              GBIFPGM 
00916                  WS-SOVA-VALQUAL5-SW                              GBIFPGM 
00917                                                                   GBIFPGM 
00918      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
00919         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00920         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00921         PERFORM 4091-BLD-SPEC-OV-COPAY                            GBIFPGM 
00922           VARYING GAF-INDEX                                       GBIFPGM 
00923             FROM 1 BY 1                                           GBIFPGM 
00924               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00925      IF WS-CON-ACP-FOUND                                          GBIFPGM 
00926         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00927         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00928         PERFORM 4091-BLD-SPEC-OV-COPAY                            GBIFPGM 
00929           VARYING GAF-INDEX                                       GBIFPGM 
00930             FROM 1 BY 1                                           GBIFPGM 
00931               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00932                                                                   GBIFPGM 
00933      IF SOVI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00934         SOVO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00935         SOVA-VALQUAL5-SW-OFF                                      GBIFPGM 
00936         PERFORM 4000-4091A.                                       GBIFPGM 
00937                                                                   GBIFPGM 
00938 *MQ 10/09/03                                                      GBIFPGM 
00939  4000-4091A.                                                      GBIFPGM 
00940                                                                   GBIFPGM 
00941      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00942         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00943         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00944         PERFORM 4091A-BLD-SPEC-OV-COPAY                           GBIFPGM 
00945           VARYING GAC-INDEX                                       GBIFPGM 
00946             FROM 1 BY 1                                           GBIFPGM 
00947               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00948      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00949         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00950         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00951         PERFORM 4091A-BLD-SPEC-OV-COPAY                           GBIFPGM 
00952           VARYING GAC-INDEX                                       GBIFPGM 
00953             FROM 1 BY 1                                           GBIFPGM 
00954               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00955                                                                   GBIFPGM 
00956 *MQ 10/09/03                                                      GBIFPGM 
00957  4000-4092.                                                       GBIFPGM 
00958                                                                   GBIFPGM 
00959      MOVE 'N' TO WS-SCOI-VALQUAL5-SW                              GBIFPGM 
00960                  WS-SCOO-VALQUAL5-SW                              GBIFPGM 
00961                  WS-SCOA-VALQUAL5-SW                              GBIFPGM 
00962                                                                   GBIFPGM 
00963      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
00964         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00965         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00966         PERFORM 4092-BLD-OP-SURG-COPAY                            GBIFPGM 
00967           VARYING GAF-INDEX                                       GBIFPGM 
00968             FROM 1 BY 1                                           GBIFPGM 
00969               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00970      IF WS-CON-ACP-FOUND                                          GBIFPGM 
00971         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00972         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
00973         PERFORM 4092-BLD-OP-SURG-COPAY                            GBIFPGM 
00974           VARYING GAF-INDEX                                       GBIFPGM 
00975             FROM 1 BY 1                                           GBIFPGM 
00976               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
00977                                                                   GBIFPGM 
00978      IF SCOI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00979         SCOO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
00980         SCOA-VALQUAL5-SW-OFF                                      GBIFPGM 
00981         PERFORM 4000-4092A.                                       GBIFPGM 
00982                                                                   GBIFPGM 
00983 *MQ 10/09/03                                                      GBIFPGM 
00984  4000-4092A.                                                      GBIFPGM 
00985                                                                   GBIFPGM 
00986      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
00987         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00988         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00989         PERFORM 4092A-BLD-OP-SURG-COPAY                           GBIFPGM 
00990           VARYING GAC-INDEX                                       GBIFPGM 
00991             FROM 1 BY 1                                           GBIFPGM 
00992               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
00993      IF WS-CON-ADL-FOUND                                          GBIFPGM 
00994         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
00995         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
00996         PERFORM 4092A-BLD-OP-SURG-COPAY                           GBIFPGM 
00997           VARYING GAC-INDEX                                       GBIFPGM 
00998             FROM 1 BY 1                                           GBIFPGM 
00999               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01000                                                                   GBIFPGM 
01001 *MQ 10/09/03                                                      GBIFPGM 
01002  4000-4093.                                                       GBIFPGM 
01003                                                                   GBIFPGM 
01004      MOVE 'N' TO WS-MSCI-VALQUAL5-SW                              GBIFPGM 
01005                  WS-MSCO-VALQUAL5-SW                              GBIFPGM 
01006                  WS-MSCA-VALQUAL5-SW                              GBIFPGM 
01007                                                                   GBIFPGM 
01008      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
01009         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01010         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01011         PERFORM 4093-BLD-OP-MSA-COPAY                             GBIFPGM 
01012           VARYING GAF-INDEX                                       GBIFPGM 
01013             FROM 1 BY 1                                           GBIFPGM 
01014               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01015      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01016         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01017         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01018         PERFORM 4093-BLD-OP-MSA-COPAY                             GBIFPGM 
01019           VARYING GAF-INDEX                                       GBIFPGM 
01020             FROM 1 BY 1                                           GBIFPGM 
01021               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01022                                                                   GBIFPGM 
01023      IF MSCI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01024         MSCO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01025         MSCA-VALQUAL5-SW-OFF                                      GBIFPGM 
01026         PERFORM 4000-4093A.                                       GBIFPGM 
01027                                                                   GBIFPGM 
01028 *MQ 10/09/03                                                      GBIFPGM 
01029  4000-4093A.                                                      GBIFPGM 
01030                                                                   GBIFPGM 
01031      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01032         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01033         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01034         PERFORM 4093A-BLD-OP-MSA-COPAY                            GBIFPGM 
01035           VARYING GAC-INDEX                                       GBIFPGM 
01036             FROM 1 BY 1                                           GBIFPGM 
01037               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01038      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01039         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01040         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01041         PERFORM 4093A-BLD-OP-MSA-COPAY                            GBIFPGM 
01042           VARYING GAC-INDEX                                       GBIFPGM 
01043             FROM 1 BY 1                                           GBIFPGM 
01044               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01045                                                                   GBIFPGM 
01046 *MQ 10/09/03                                                      GBIFPGM 
01047  4000-4094.                                                       GBIFPGM 
01048                                                                   GBIFPGM 
01049      MOVE 'N' TO WS-UCCI-VALQUAL5-SW                              GBIFPGM 
01050                  WS-UCCO-VALQUAL5-SW                              GBIFPGM 
01051                  WS-UCCA-VALQUAL5-SW                              GBIFPGM 
01052                                                                   GBIFPGM 
01053      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
01054         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01055         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01056         PERFORM 4094-BLD-UCF-COPAY                                GBIFPGM 
01057           VARYING GAF-INDEX                                       GBIFPGM 
01058             FROM 1 BY 1                                           GBIFPGM 
01059               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01060      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01061         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01062         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01063         PERFORM 4094-BLD-UCF-COPAY                                GBIFPGM 
01064           VARYING GAF-INDEX                                       GBIFPGM 
01065             FROM 1 BY 1                                           GBIFPGM 
01066               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01067                                                                   GBIFPGM 
01068      IF UCCI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01069         UCCO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01070         UCCA-VALQUAL5-SW-OFF                                      GBIFPGM 
01071         PERFORM 4000-4094A.                                       GBIFPGM 
01072                                                                   GBIFPGM 
01073 *MQ 10/09/03                                                      GBIFPGM 
01074  4000-4094A.                                                      GBIFPGM 
01075                                                                   GBIFPGM 
01076      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01077         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01078         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01079         PERFORM 4094A-BLD-UCF-COPAY                               GBIFPGM 
01080           VARYING GAC-INDEX                                       GBIFPGM 
01081             FROM 1 BY 1                                           GBIFPGM 
01082               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01083      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01084         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01085         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01086         PERFORM 4094A-BLD-UCF-COPAY                               GBIFPGM 
01087           VARYING GAC-INDEX                                       GBIFPGM 
01088             FROM 1 BY 1                                           GBIFPGM 
01089               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01090                                                                   GBIFPGM 
01091 *MQ 10/09/03                                                      GBIFPGM 
01092  4000-4095.                                                       GBIFPGM 
01093                                                                   GBIFPGM 
01094      MOVE 'N' TO WS-OPFI-VALQUAL5-SW                              GBIFPGM 
01095                  WS-OPFO-VALQUAL5-SW                              GBIFPGM 
01096                  WS-OPFA-VALQUAL5-SW                              GBIFPGM 
01097                                                                   GBIFPGM 
01098      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
01099         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01100         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01101         PERFORM 4095-BLD-OP-HOSP-COPAY                            GBIFPGM 
01102           VARYING GAF-INDEX                                       GBIFPGM 
01103             FROM 1 BY 1                                           GBIFPGM 
01104               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01105      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01106         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01107         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01108         PERFORM 4095-BLD-OP-HOSP-COPAY                            GBIFPGM 
01109           VARYING GAF-INDEX                                       GBIFPGM 
01110             FROM 1 BY 1                                           GBIFPGM 
01111               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01112                                                                   GBIFPGM 
01113      IF OPFI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01114         OPFO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01115         OPFA-VALQUAL5-SW-OFF                                      GBIFPGM 
01116         PERFORM 4000-4095A.                                       GBIFPGM 
01117                                                                   GBIFPGM 
01118 *MQ 10/09/03                                                      GBIFPGM 
01119  4000-4095A.                                                      GBIFPGM 
01120                                                                   GBIFPGM 
01121      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01122         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01123         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01124         PERFORM 4095A-BLD-OP-HOSP-COPAY                           GBIFPGM 
01125           VARYING GAC-INDEX                                       GBIFPGM 
01126             FROM 1 BY 1                                           GBIFPGM 
01127               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01128      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01129         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01130         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01131         PERFORM 4095A-BLD-OP-HOSP-COPAY                           GBIFPGM 
01132           VARYING GAC-INDEX                                       GBIFPGM 
01133             FROM 1 BY 1                                           GBIFPGM 
01134               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01135                                                                   GBIFPGM 
01136 *MQ 10/09/03                                                      GBIFPGM 
01137  4000-4096.                                                       GBIFPGM 
01138                                                                   GBIFPGM 
01139      MOVE 'N' TO WS-UCPI-VALQUAL5-SW                              GBIFPGM 
01140                  WS-UCPO-VALQUAL5-SW                              GBIFPGM 
01141                  WS-UCPA-VALQUAL5-SW                              GBIFPGM 
01142                                                                   GBIFPGM 
01143      IF WS-GRP-ACP-FOUND                                          GBIFPGM 
01144         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01145         MOVE WS-GRP-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01146         PERFORM 4096-BLD-UCP-COPAY                                GBIFPGM 
01147           VARYING GAF-INDEX                                       GBIFPGM 
01148             FROM 1 BY 1                                           GBIFPGM 
01149               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01150      IF WS-CON-ACP-FOUND                                          GBIFPGM 
01151         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01152         MOVE WS-CON-ACP-HOLD TO WS-LOOP-ACP-HOLD                  GBIFPGM 
01153         PERFORM 4096-BLD-UCP-COPAY                                GBIFPGM 
01154           VARYING GAF-INDEX                                       GBIFPGM 
01155             FROM 1 BY 1                                           GBIFPGM 
01156               UNTIL GAF-INDEX = GAF-ENTRY-COUNT.                  GBIFPGM 
01157                                                                   GBIFPGM 
01158      IF UCPI-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01159         UCPO-VALQUAL5-SW-OFF OR                                   GBIFPGM 
01160         UCPA-VALQUAL5-SW-OFF                                      GBIFPGM 
01161         PERFORM 4000-4096A.                                       GBIFPGM 
01162                                                                   GBIFPGM 
01163 *MQ 10/09/03                                                      GBIFPGM 
01164  4000-4096A.                                                      GBIFPGM 
01165                                                                   GBIFPGM 
01166      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01167         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01168         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01169         PERFORM 4096A-BLD-UCP-COPAY                               GBIFPGM 
01170           VARYING GAC-INDEX                                       GBIFPGM 
01171             FROM 1 BY 1                                           GBIFPGM 
01172               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01173      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01174         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01175         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01176         PERFORM 4096A-BLD-UCP-COPAY                               GBIFPGM 
01177           VARYING GAC-INDEX                                       GBIFPGM 
01178             FROM 1 BY 1                                           GBIFPGM 
01179               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01180                                                                   GBIFPGM 
01181  4000-4100.                                                       GBIFPGM 
01182                                                                   GBIFPGM 
01183      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01184         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01185         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01186         PERFORM 4100-BLD-HOSP-PMT-LVL                             GBIFPGM 
01187           VARYING GAB-INDEX                                       GBIFPGM 
01188             FROM 1 BY 1                                           GBIFPGM 
01189               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01190      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01191         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01192         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01193         PERFORM 4100-BLD-HOSP-PMT-LVL                             GBIFPGM 
01194           VARYING GAB-INDEX                                       GBIFPGM 
01195             FROM 1 BY 1                                           GBIFPGM 
01196               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01197                                                                   GBIFPGM 
01198  4000-4110.                                                       GBIFPGM 
01199                                                                   GBIFPGM 
01200      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01201         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01202         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01203         PERFORM 4110-BLD-PER-ADM-DED                              GBIFPGM 
01204           VARYING GAC-INDEX                                       GBIFPGM 
01205             FROM 1 BY 1                                           GBIFPGM 
01206               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01207      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01208         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01209         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01210         PERFORM 4110-BLD-PER-ADM-DED                              GBIFPGM 
01670           VARYING GAC-INDEX                                       GBIFPGM 
01671             FROM 1 BY 1                                           GBIFPGM 
01672               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01673                                                                   GBIFPGM 
01674 *MQ 05/03/05                                                      GBIFPGM 
01675  4000-4115.                                                       GBIFPGM 
01676                                                                   GBIFPGM 
01677      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01678         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01679         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01680         PERFORM 4115-BLD-PER-ADM-DED-MAX                          GBIFPGM 
01681           VARYING GAC-INDEX                                       GBIFPGM 
01682             FROM 1 BY 1                                           GBIFPGM 
01683               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01684      IF WS-CON-ADL-FOUND                                          GBIFPGM 
01685         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01686         MOVE WS-CON-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01687         PERFORM 4115-BLD-PER-ADM-DED-MAX                          GBIFPGM 
01211           VARYING GAC-INDEX                                       GBIFPGM 
01212             FROM 1 BY 1                                           GBIFPGM 
01213               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01214                                                                   GBIFPGM 
01215  4000-4120.                                                       GBIFPGM 
01216                                                                   GBIFPGM 
01217      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01218         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01219         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01220         PERFORM 4120-BLD-OS-HOS-PMT-LVL                           GBIFPGM 
01221           VARYING GAB-INDEX                                       GBIFPGM 
01222             FROM 1 BY 1                                           GBIFPGM 
01223               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01224      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01225         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01226         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01227         PERFORM 4120-BLD-OS-HOS-PMT-LVL                           GBIFPGM 
01228           VARYING GAB-INDEX                                       GBIFPGM 
01229             FROM 1 BY 1                                           GBIFPGM 
01230               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01231                                                                   GBIFPGM 
01232  4000-4130.                                                       GBIFPGM 
01233                                                                   GBIFPGM 
01234      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01235         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01236         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01237         PERFORM 4130-BLD-OS-PRF-PMT-LVL                           GBIFPGM 
01238           VARYING GAB-INDEX                                       GBIFPGM 
01239             FROM 1 BY 1                                           GBIFPGM 
01240               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01241      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01242         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01243         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01244         PERFORM 4130-BLD-OS-PRF-PMT-LVL                           GBIFPGM 
01245           VARYING GAB-INDEX                                       GBIFPGM 
01246             FROM 1 BY 1                                           GBIFPGM 
01247               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01248                                                                   GBIFPGM 
01249 *MQ 10/03                                                         GBIFPGM 
01250  4000-4135.                                                       GBIFPGM 
01251                                                                   GBIFPGM 
01252      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01253         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01254         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01255         PERFORM 4135-BLD-OS-BCBS-PMT-LVL                          GBIFPGM 
01256           VARYING GAB-INDEX                                       GBIFPGM 
01257             FROM 1 BY 1                                           GBIFPGM 
01258               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01259      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01260         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01261         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01262         PERFORM 4135-BLD-OS-BCBS-PMT-LVL                          GBIFPGM 
01263           VARYING GAB-INDEX                                       GBIFPGM 
01264             FROM 1 BY 1                                           GBIFPGM 
01265               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01266                                                                   GBIFPGM 
01267  4000-4140.                                                       GBIFPGM 
01268                                                                   GBIFPGM 
01269      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01270         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01271         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01272         PERFORM 4140-BLD-OD-HOS-PMT-LVL                           GBIFPGM 
01273           VARYING GAB-INDEX                                       GBIFPGM 
01274             FROM 1 BY 1                                           GBIFPGM 
01275               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01276      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01277         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01278         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01279         PERFORM 4140-BLD-OD-HOS-PMT-LVL                           GBIFPGM 
01280           VARYING GAB-INDEX                                       GBIFPGM 
01281             FROM 1 BY 1                                           GBIFPGM 
01282               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01283                                                                   GBIFPGM 
01284  4000-4150.                                                       GBIFPGM 
01285                                                                   GBIFPGM 
01286      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01287         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01288         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01289         PERFORM 4150-BLD-OD-PRF-PMT-LVL                           GBIFPGM 
01290           VARYING GAB-INDEX                                       GBIFPGM 
01291             FROM 1 BY 1                                           GBIFPGM 
01292               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01293      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01294         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01295         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01296         PERFORM 4150-BLD-OD-PRF-PMT-LVL                           GBIFPGM 
01297           VARYING GAB-INDEX                                       GBIFPGM 
01298             FROM 1 BY 1                                           GBIFPGM 
01299               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01300                                                                   GBIFPGM 
01301  4000-4160.                                                       GBIFPGM 
01302                                                                   GBIFPGM 
01303      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01304         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01305         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01306         PERFORM 4160-BLD-EMER-ACID-HOSP                           GBIFPGM 
01307           VARYING GAB-INDEX                                       GBIFPGM 
01308             FROM 1 BY 1                                           GBIFPGM 
01309               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01310      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01311         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01312         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01313         PERFORM 4160-BLD-EMER-ACID-HOSP                           GBIFPGM 
01314           VARYING GAB-INDEX                                       GBIFPGM 
01315             FROM 1 BY 1                                           GBIFPGM 
01316               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01317                                                                   GBIFPGM 
01318  4000-4170.                                                       GBIFPGM 
01319                                                                   GBIFPGM 
01320      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01321         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01322         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01323         PERFORM 4170-BLD-EMER-ACID-PROF                           GBIFPGM 
01324           VARYING GAB-INDEX                                       GBIFPGM 
01325             FROM 1 BY 1                                           GBIFPGM 
01326               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01327      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01328         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01329         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01330         PERFORM 4170-BLD-EMER-ACID-PROF                           GBIFPGM 
01331           VARYING GAB-INDEX                                       GBIFPGM 
01332             FROM 1 BY 1                                           GBIFPGM 
01333               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01334                                                                   GBIFPGM 
01335  4000-4175.                                                       GBIFPGM 
01336 *MQ 10/03                                                         GBIFPGM 
01337                                                                   GBIFPGM 
01338      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01339         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01340         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01341         PERFORM 4175-BLD-EAC-BCBS-PMT-LVL                         GBIFPGM 
01342           VARYING GAB-INDEX                                       GBIFPGM 
01343             FROM 1 BY 1                                           GBIFPGM 
01344               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01345      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01346         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01347         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01348         PERFORM 4175-BLD-EAC-BCBS-PMT-LVL                         GBIFPGM 
01349           VARYING GAB-INDEX                                       GBIFPGM 
01350             FROM 1 BY 1                                           GBIFPGM 
01351               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01352                                                                   GBIFPGM 
01353  4000-4180.                                                       GBIFPGM 
01354                                                                   GBIFPGM 
01355      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01356         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01357         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01358         PERFORM 4180-BLD-EMC-HOSP-PMT                             GBIFPGM 
01359           VARYING GAB-INDEX                                       GBIFPGM 
01360             FROM 1 BY 1                                           GBIFPGM 
01361               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01362      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01363         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01364         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01365         PERFORM 4180-BLD-EMC-HOSP-PMT                             GBIFPGM 
01366           VARYING GAB-INDEX                                       GBIFPGM 
01367             FROM 1 BY 1                                           GBIFPGM 
01368               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01369                                                                   GBIFPGM 
01370  4000-4190.                                                       GBIFPGM 
01371                                                                   GBIFPGM 
01372      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01373         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01374         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01375         PERFORM 4190-BLD-EMC-PROF-PMT                             GBIFPGM 
01376           VARYING GAB-INDEX                                       GBIFPGM 
01377             FROM 1 BY 1                                           GBIFPGM 
01378               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01379      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01380         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01381         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01382         PERFORM 4190-BLD-EMC-PROF-PMT                             GBIFPGM 
01383           VARYING GAB-INDEX                                       GBIFPGM 
01384             FROM 1 BY 1                                           GBIFPGM 
01385               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01386                                                                   GBIFPGM 
01387 *MQ 10/03                                                         GBIFPGM 
01388  4000-4195.                                                       GBIFPGM 
01389                                                                   GBIFPGM 
01390      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01391         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01392         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01393         PERFORM 4195-BLD-EMC-BCBS-PMT-LVL                         GBIFPGM 
01394           VARYING GAB-INDEX                                       GBIFPGM 
01395             FROM 1 BY 1                                           GBIFPGM 
01396               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01397      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01398         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01399         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01400         PERFORM 4195-BLD-EMC-BCBS-PMT-LVL                         GBIFPGM 
01401           VARYING GAB-INDEX                                       GBIFPGM 
01402             FROM 1 BY 1                                           GBIFPGM 
01403               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01404                                                                   GBIFPGM 
01405 *MQ 10/03                                                         GBIFPGM 
01406  4000-4196.                                                       GBIFPGM 
01407                                                                   GBIFPGM 
01408      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01409         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01410         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01411         PERFORM 4196-BLD-EAC-EMC-BC-PMT-LVL                       GBIFPGM 
01412           VARYING GAB-INDEX                                       GBIFPGM 
01413             FROM 1 BY 1                                           GBIFPGM 
01414               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01415      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01416         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01417         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01418         PERFORM 4196-BLD-EAC-EMC-BC-PMT-LVL                       GBIFPGM 
01419           VARYING GAB-INDEX                                       GBIFPGM 
01420             FROM 1 BY 1                                           GBIFPGM 
01421               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01422                                                                   GBIFPGM 
01423 *MQ 10/03                                                         GBIFPGM 
01424  4000-4197.                                                       GBIFPGM 
01425                                                                   GBIFPGM 
01426      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01427         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01428         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01429         PERFORM 4197-BLD-EAC-EMC-BS-PMT-LVL                       GBIFPGM 
01430           VARYING GAB-INDEX                                       GBIFPGM 
01431             FROM 1 BY 1                                           GBIFPGM 
01432               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01433      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01434         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01435         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01436         PERFORM 4197-BLD-EAC-EMC-BS-PMT-LVL                       GBIFPGM 
01437           VARYING GAB-INDEX                                       GBIFPGM 
01438             FROM 1 BY 1                                           GBIFPGM 
01439               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01440                                                                   GBIFPGM 
01441 *MQ 10/03                                                         GBIFPGM 
01442  4000-4198.                                                       GBIFPGM 
01443                                                                   GBIFPGM 
01444      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01445         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01446         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01447         PERFORM 4198-BLD-EAC-EMC-BCBS-PMT-LVL                     GBIFPGM 
01448           VARYING GAB-INDEX                                       GBIFPGM 
01449             FROM 1 BY 1                                           GBIFPGM 
01450               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01451      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01452         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01453         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01454         PERFORM 4198-BLD-EAC-EMC-BCBS-PMT-LVL                     GBIFPGM 
01455           VARYING GAB-INDEX                                       GBIFPGM 
01456             FROM 1 BY 1                                           GBIFPGM 
01457               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01458                                                                   GBIFPGM 
01459  4000-4200.                                                       GBIFPGM 
01460                                                                   GBIFPGM 
01461      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01462         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01463         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01464         PERFORM 4200-BLD-SUPP-ACCID                               GBIFPGM 
01465           VARYING GAB-INDEX                                       GBIFPGM 
01466             FROM 1 BY 1                                           GBIFPGM 
01467               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01468      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01469         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01470         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01471         PERFORM 4200-BLD-SUPP-ACCID                               GBIFPGM 
01472           VARYING GAB-INDEX                                       GBIFPGM 
01473             FROM 1 BY 1                                           GBIFPGM 
01474               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01475                                                                   GBIFPGM 
01476  4000-4210.                                                       GBIFPGM 
01477                                                                   GBIFPGM 
01478      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01479         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01480         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01481         PERFORM 4210-BLD-MS-PMT-LVL                               GBIFPGM 
01482           VARYING GAB-INDEX                                       GBIFPGM 
01483             FROM 1 BY 1                                           GBIFPGM 
01484               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01485      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01486         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01487         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01488         PERFORM 4210-BLD-MS-PMT-LVL                               GBIFPGM 
01489           VARYING GAB-INDEX                                       GBIFPGM 
01490             FROM 1 BY 1                                           GBIFPGM 
01491               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01492                                                                   GBIFPGM 
01493 *MQ 12/03/03                                                      GBIFPGM 
01494  4000-4215.                                                       GBIFPGM 
01495                                                                   GBIFPGM 
01496      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01497         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01498         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01499         PERFORM 4215-BLD-HOSP-MS-PMT-LVL                          GBIFPGM 
01500           VARYING GAB-INDEX                                       GBIFPGM 
01501             FROM 1 BY 1                                           GBIFPGM 
01502               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01503      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01504         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01505         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01506         PERFORM 4215-BLD-HOSP-MS-PMT-LVL                          GBIFPGM 
01507           VARYING GAB-INDEX                                       GBIFPGM 
01508             FROM 1 BY 1                                           GBIFPGM 
01509               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01510                                                                   GBIFPGM 
01511  4000-4220.                                                       GBIFPGM 
01512                                                                   GBIFPGM 
01513      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01514         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01515         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01516         PERFORM 4220-BLD-THERAPY-MAX-COMB                         GBIFPGM 
01517           VARYING GAA-INDEX                                       GBIFPGM 
01518             FROM 1 BY 1                                           GBIFPGM 
01519               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01520      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01521         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01522         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01523         PERFORM 4220-BLD-THERAPY-MAX-COMB                         GBIFPGM 
01524           VARYING GAA-INDEX                                       GBIFPGM 
01525             FROM 1 BY 1                                           GBIFPGM 
01526               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01527                                                                   GBIFPGM 
01528  4000-4230.                                                       GBIFPGM 
01529                                                                   GBIFPGM 
01530      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01531         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01532         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01533         PERFORM 4230-BLD-FUNC-OCC-THER-MAX                        GBIFPGM 
01534           VARYING GAA-INDEX                                       GBIFPGM 
01535             FROM 1 BY 1                                           GBIFPGM 
01536               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01537      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01538         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01539         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01540         PERFORM 4230-BLD-FUNC-OCC-THER-MAX                        GBIFPGM 
01541           VARYING GAA-INDEX                                       GBIFPGM 
01542             FROM 1 BY 1                                           GBIFPGM 
01543               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01544                                                                   GBIFPGM 
01545  4000-4240.                                                       GBIFPGM 
01546                                                                   GBIFPGM 
01547      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01548         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01549         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01550         PERFORM 4240-BLD-PHYS-MECH-THER-MAX                       GBIFPGM 
01551           VARYING GAA-INDEX                                       GBIFPGM 
01552             FROM 1 BY 1                                           GBIFPGM 
01553               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01554      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01555         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01556         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01557         PERFORM 4240-BLD-PHYS-MECH-THER-MAX                       GBIFPGM 
01558           VARYING GAA-INDEX                                       GBIFPGM 
01559             FROM 1 BY 1                                           GBIFPGM 
01560               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01561                                                                   GBIFPGM 
01562  4000-4250.                                                       GBIFPGM 
01563                                                                   GBIFPGM 
01564      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01565         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01566         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01567         PERFORM 4250-BLD-SPEECH-THER-MAX                          GBIFPGM 
01568           VARYING GAA-INDEX                                       GBIFPGM 
01569             FROM 1 BY 1                                           GBIFPGM 
01570               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01571      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01572         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01573         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01574         PERFORM 4250-BLD-SPEECH-THER-MAX                          GBIFPGM 
01575           VARYING GAA-INDEX                                       GBIFPGM 
01576             FROM 1 BY 1                                           GBIFPGM 
01577               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01578                                                                   GBIFPGM 
01579  4000-4260.                                                       GBIFPGM 
01580                                                                   GBIFPGM 
01581      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01582         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01583         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01584         PERFORM 4260-BLD-TMJ-LIFETIME-MAX                         GBIFPGM 
01585           VARYING GAA-INDEX                                       GBIFPGM 
01586             FROM 1 BY 1                                           GBIFPGM 
01587               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01588      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01589         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01590         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01591         PERFORM 4260-BLD-TMJ-LIFETIME-MAX                         GBIFPGM 
01592           VARYING GAA-INDEX                                       GBIFPGM 
01593             FROM 1 BY 1                                           GBIFPGM 
01594               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01595                                                                   GBIFPGM 
01596  4000-4270.                                                       GBIFPGM 
01597                                                                   GBIFPGM 
01598      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01599         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01600         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01601         PERFORM 4270-BLD-PRIV-DUTY-NURSE-MAX                      GBIFPGM 
01602           VARYING GAA-INDEX                                       GBIFPGM 
01603             FROM 1 BY 1                                           GBIFPGM 
01604               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01605      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01606         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01607         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01608         PERFORM 4270-BLD-PRIV-DUTY-NURSE-MAX                      GBIFPGM 
01609           VARYING GAA-INDEX                                       GBIFPGM 
01610             FROM 1 BY 1                                           GBIFPGM 
01611               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01612                                                                   GBIFPGM 
01613 *MQ 10/03                                                         GBIFPGM 
01614  4000-4275.                                                       GBIFPGM 
01615                                                                   GBIFPGM 
01616      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01617         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01618         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01619         PERFORM 4275-BLD-SKILL-NUR-BP-MAX                         GBIFPGM 
01620           VARYING GAA-INDEX                                       GBIFPGM 
01621             FROM 1 BY 1                                           GBIFPGM 
01622               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01623      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01624         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01625         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01626         PERFORM 4275-BLD-SKILL-NUR-BP-MAX                         GBIFPGM 
01627           VARYING GAA-INDEX                                       GBIFPGM 
01628             FROM 1 BY 1                                           GBIFPGM 
01629               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01630                                                                   GBIFPGM 
01631  4000-4280.                                                       GBIFPGM 
01632                                                                   GBIFPGM 
01633      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01634         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01635         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01636         PERFORM 4280-BLD-CHIRO-SERV-MAX                           GBIFPGM 
01637           VARYING GAA-INDEX                                       GBIFPGM 
01638             FROM 1 BY 1                                           GBIFPGM 
01639               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01640      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01641         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01642         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01643         PERFORM 4280-BLD-CHIRO-SERV-MAX                           GBIFPGM 
01644           VARYING GAA-INDEX                                       GBIFPGM 
01645             FROM 1 BY 1                                           GBIFPGM 
01646               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01647                                                                   GBIFPGM 
01648  4000-4290.                                                       GBIFPGM 
01649                                                                   GBIFPGM 
01650      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01651         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01652         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01653         PERFORM 4290-BLD-CHIRO-PROV-MAX                           GBIFPGM 
01654           VARYING GAA-INDEX                                       GBIFPGM 
01655             FROM 1 BY 1                                           GBIFPGM 
01656               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01657      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01658         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01659         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01660         PERFORM 4290-BLD-CHIRO-PROV-MAX                           GBIFPGM 
01661           VARYING GAA-INDEX                                       GBIFPGM 
01662             FROM 1 BY 1                                           GBIFPGM 
01663               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01664                                                                   GBIFPGM 
01665  4000-4300.                                                       GBIFPGM 
01666                                                                   GBIFPGM 
01667      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01668         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01669         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01670         PERFORM 4300-BLD-WELL-CARE-PMT-LVL                        GBIFPGM 
01671           VARYING GAB-INDEX                                       GBIFPGM 
01672             FROM 1 BY 1                                           GBIFPGM 
01673               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01674      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01675         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01676         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01677         PERFORM 4300-BLD-WELL-CARE-PMT-LVL                        GBIFPGM 
01678           VARYING GAB-INDEX                                       GBIFPGM 
01679             FROM 1 BY 1                                           GBIFPGM 
01680               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01681                                                                   GBIFPGM 
01682 *MQ 10/03                                                         GBIFPGM 
01683  4000-4305.                                                       GBIFPGM 
01684                                                                   GBIFPGM 
01685      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01686         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01687         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01688         PERFORM 4305-BLD-WELL-AD-CARE-PMT-LVL                     GBIFPGM 
01689           VARYING GAB-INDEX                                       GBIFPGM 
01690             FROM 1 BY 1                                           GBIFPGM 
01691               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01692      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01693         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01694         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01695         PERFORM 4305-BLD-WELL-AD-CARE-PMT-LVL                     GBIFPGM 
01696           VARYING GAB-INDEX                                       GBIFPGM 
01697             FROM 1 BY 1                                           GBIFPGM 
01698               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01699                                                                   GBIFPGM 
01700  4000-4310.                                                       GBIFPGM 
01701                                                                   GBIFPGM 
01702      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01703         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01704         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01705         PERFORM 4310-BLD-WELL-ADLT-CARE-MAX                       GBIFPGM 
01706           VARYING GAA-INDEX                                       GBIFPGM 
01707             FROM 1 BY 1                                           GBIFPGM 
01708               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01709      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01710         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01711         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01712         PERFORM 4310-BLD-WELL-ADLT-CARE-MAX                       GBIFPGM 
01713           VARYING GAA-INDEX                                       GBIFPGM 
01714             FROM 1 BY 1                                           GBIFPGM 
01715               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01716                                                                   GBIFPGM 
01717 *MQ 10/03                                                         GBIFPGM 
01718  4000-4315.                                                       GBIFPGM 
01719                                                                   GBIFPGM 
01720      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01721         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01722         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01723         PERFORM 4315-BLD-WELL-AD-CARE-BP-MAX                      GBIFPGM 
01724           VARYING GAA-INDEX                                       GBIFPGM 
01725             FROM 1 BY 1                                           GBIFPGM 
01726               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01727      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01728         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01729         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01730         PERFORM 4315-BLD-WELL-AD-CARE-BP-MAX                      GBIFPGM 
01731           VARYING GAA-INDEX                                       GBIFPGM 
01732             FROM 1 BY 1                                           GBIFPGM 
01733               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01734                                                                   GBIFPGM 
01735                                                                   GBIFPGM 
01736  4000-4320.                                                       GBIFPGM 
01737                                                                   GBIFPGM 
01738      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01739         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01740         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01741         PERFORM 4320-BLD-WELL-CHILD-CARE-MAX                      GBIFPGM 
01742           VARYING GAA-INDEX                                       GBIFPGM 
01743             FROM 1 BY 1                                           GBIFPGM 
01744               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01745      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01746         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01747         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01748         PERFORM 4320-BLD-WELL-CHILD-CARE-MAX                      GBIFPGM 
01749           VARYING GAA-INDEX                                       GBIFPGM 
01750             FROM 1 BY 1                                           GBIFPGM 
01751               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01752                                                                   GBIFPGM 
01753  4000-4325.                                                       GBIFPGM 
01754                                                                   GBIFPGM 
01755      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01756         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01757         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01758         PERFORM 4325-BLD-WELL-CH-CARE-PMT-LVL                     GBIFPGM 
01759           VARYING GAB-INDEX                                       GBIFPGM 
01760             FROM 1 BY 1                                           GBIFPGM 
01761               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01762      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01763         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01764         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01765         PERFORM 4325-BLD-WELL-CH-CARE-PMT-LVL                     GBIFPGM 
01766           VARYING GAB-INDEX                                       GBIFPGM 
01767             FROM 1 BY 1                                           GBIFPGM 
01768               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01769                                                                   GBIFPGM 
01770  4000-4330.                                                       GBIFPGM 
01771                                                                   GBIFPGM 
01772      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01773         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01774         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01775         PERFORM 4330-BLD-OTHER-CS-PMT-LVL                         GBIFPGM 
01776           VARYING GAB-INDEX                                       GBIFPGM 
01777             FROM 1 BY 1                                           GBIFPGM 
01778               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01779      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01780         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01781         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01782         PERFORM 4330-BLD-OTHER-CS-PMT-LVL                         GBIFPGM 
01783           VARYING GAB-INDEX                                       GBIFPGM 
01784             FROM 1 BY 1                                           GBIFPGM 
01785               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01786                                                                   GBIFPGM 
01787  4000-4335.                                                       GBIFPGM 
01788                                                                   GBIFPGM 
01789      MOVE GCG-DEP-MAX-AGE TO WS-AGE.                              GBIFPGM 
01790      MOVE WS-AGE          TO GCBH-DEPENDENT-MAX-AGE.              GBIFPGM 
01791                                                                   GBIFPGM 
01792  4000-4336.                                                       GBIFPGM 
01793                                                                   GBIFPGM 
01794      MOVE GCG-STU-MAX-AGE TO WS-AGE.                              GBIFPGM 
01795      MOVE WS-AGE          TO GCBH-STUDENT-MAX-AGE.                GBIFPGM 
01796                                                                   GBIFPGM 
01797                                                                   GBIFPGM 
01798  4000-4337.                                                       GBIFPGM 
01799                                                                   GBIFPGM 
01800      MOVE GCG-BC-WAITG-PERD-MEM-DAYS                              GBIFPGM 
01801                                  TO  GCBH-WAITING-PER-MEM.        GBIFPGM 
01802      MOVE GCG-BC-WAITG-PERD-SPS-DAYS                              GBIFPGM 
01803                                  TO  GCBH-WAITING-PER-SPS.        GBIFPGM 
01804      MOVE GCG-BC-WAITG-PERD-DEP-DAYS                              GBIFPGM 
01805                                  TO  GCBH-WAITING-PER-DEP.        GBIFPGM 
01806                                                                   GBIFPGM 
01807  4000-4340.                                                       GBIFPGM 
01808                                                                   GBIFPGM 
01809      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01810         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01811         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01812         PERFORM 4340-BLD-MSA-SANCTION-COINS                       GBIFPGM 
01813           VARYING GAB-INDEX                                       GBIFPGM 
01814             FROM 1 BY 1                                           GBIFPGM 
01815               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01816                                                                   GBIFPGM 
01817  4000-4350.                                                       GBIFPGM 
01818                                                                   GBIFPGM 
02296      MOVE 'N' TO WS-MSID-VALQUAL5-SW                              GBIFPGM 
02297                  WS-MSOD-VALQUAL5-SW                              GBIFPGM 
02298                  WS-MSAD-VALQUAL5-SW                              GBIFPGM 
01820                                                                   GBIFPGM 
01821      IF WS-GRP-ADL-FOUND                                          GBIFPGM 
01822         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01823         MOVE WS-GRP-ADL-HOLD TO WS-LOOP-ADL-HOLD                  GBIFPGM 
01824         PERFORM 4350-BLD-MSA-SANCTION-DED                         GBIFPGM 
01825           VARYING GAC-INDEX                                       GBIFPGM 
01826             FROM 1 BY 1                                           GBIFPGM 
01827               UNTIL GAC-INDEX = GAC-ENTRY-COUNT.                  GBIFPGM 
01828                                                                   GBIFPGM 
02308      IF MSID-VALQUAL5-SW-OFF OR                                   GBIFPGM 
02309         MSOD-VALQUAL5-SW-OFF OR                                   GBIFPGM 
02310         MSAD-VALQUAL5-SW-OFF                                      GBIFPGM 
01830         PERFORM 4000-4350A.                                       GBIFPGM 
01831                                                                   GBIFPGM 
01832 *MQ 04/07/04                                                      GBIFPGM 
01833  4000-4350A.                                                      GBIFPGM 
01834                                                                   GBIFPGM 
01835      IF WS-GRP-AOL-FOUND                                          GBIFPGM 
01836         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01837         MOVE WS-GRP-AOL-HOLD TO WS-LOOP-AOL-HOLD                  GBIFPGM 
01838         PERFORM 4350A-BLD-MSA-SANCTION-DED                        GBIFPGM 
01839           VARYING GAD-INDEX                                       GBIFPGM 
01840             FROM 1 BY 1                                           GBIFPGM 
01841               UNTIL GAD-INDEX = GAD-ENTRY-COUNT.                  GBIFPGM 
01842                                                                   GBIFPGM 
01843                                                                   GBIFPGM 
01844  4000-4360.                                                       GBIFPGM 
01845 **** RESET HEADING SWITCH ****************************************GBIFPGM 
01846                                                                   GBIFPGM 
01847      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01848         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01849         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01850         PERFORM 4360-BLD-MS-ABUSE-PMT-LVL                         GBIFPGM 
01851           VARYING GAB-INDEX                                       GBIFPGM 
01852             FROM 1 BY 1                                           GBIFPGM 
01853               UNTIL GAB-INDEX = GAB-ENTRY-COUNT                   GBIFPGM 
01854      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01855         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01856         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01857         PERFORM 4360-BLD-MS-ABUSE-PMT-LVL                         GBIFPGM 
01858           VARYING GAB-INDEX                                       GBIFPGM 
01859             FROM 1 BY 1                                           GBIFPGM 
01860               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01861                                                                   GBIFPGM 
01862 *MQ 10/03                                                         GBIFPGM 
01863  4000-4370.                                                       GBIFPGM 
01864                                                                   GBIFPGM 
01865      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01866         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01867         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01868         PERFORM 4370-BLD-HEAR-AID-BP-MAX                          GBIFPGM 
01869           VARYING GAA-INDEX                                       GBIFPGM 
01870             FROM 1 BY 1                                           GBIFPGM 
01871               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01872      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01873         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01874         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01875         PERFORM 4370-BLD-HEAR-AID-BP-MAX                          GBIFPGM 
01876           VARYING GAA-INDEX                                       GBIFPGM 
01877             FROM 1 BY 1                                           GBIFPGM 
01878               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01879                                                                   GBIFPGM 
01880 *MQ 10/03                                                         GBIFPGM 
01881  4000-4380.                                                       GBIFPGM 
01882                                                                   GBIFPGM 
01883      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01884         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01885         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01886         PERFORM 4380-BLD-NON-PLAN-PMT-LVL                         GBIFPGM 
01887           VARYING GAB-INDEX                                       GBIFPGM 
01888             FROM 1 BY 1                                           GBIFPGM 
01889               UNTIL GAB-INDEX = GAB-ENTRY-COUNT                   GBIFPGM 
01890      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01891         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01892         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01893         PERFORM 4380-BLD-NON-PLAN-PMT-LVL                         GBIFPGM 
01894           VARYING GAB-INDEX                                       GBIFPGM 
01895             FROM 1 BY 1                                           GBIFPGM 
01896               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01897                                                                   GBIFPGM 
01898 *MQ 10/03                                                         GBIFPGM 
01899  4000-4390.                                                       GBIFPGM 
01900                                                                   GBIFPGM 
01901      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01902         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01903         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01904         PERFORM 4390-BLD-CON-LENS-BP-MAX                          GBIFPGM 
01905           VARYING GAA-INDEX                                       GBIFPGM 
01906             FROM 1 BY 1                                           GBIFPGM 
01907               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01908      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01909         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01910         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01911         PERFORM 4390-BLD-CON-LENS-BP-MAX                          GBIFPGM 
01912           VARYING GAA-INDEX                                       GBIFPGM 
01913             FROM 1 BY 1                                           GBIFPGM 
01914               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01915                                                                   GBIFPGM 
01916 *MQ 10/03                                                         GBIFPGM 
01917  4000-4392.                                                       GBIFPGM 
01918                                                                   GBIFPGM 
01919      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01920         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01921         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01922         PERFORM 4392-BLD-FRAME-BP-MAX                             GBIFPGM 
01923           VARYING GAA-INDEX                                       GBIFPGM 
01924             FROM 1 BY 1                                           GBIFPGM 
01925               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01926      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01927         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01928         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01929         PERFORM 4392-BLD-FRAME-BP-MAX                             GBIFPGM 
01930           VARYING GAA-INDEX                                       GBIFPGM 
01931             FROM 1 BY 1                                           GBIFPGM 
01932               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01933                                                                   GBIFPGM 
01934 *MQ 10/03                                                         GBIFPGM 
01935  4000-4394.                                                       GBIFPGM 
01936                                                                   GBIFPGM 
01937      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01938         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01939         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01940         PERFORM 4394-BLD-VIS-EX-BP-MAX                            GBIFPGM 
01941           VARYING GAA-INDEX                                       GBIFPGM 
01942             FROM 1 BY 1                                           GBIFPGM 
01943               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01944      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01945         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01946         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01947         PERFORM 4394-BLD-VIS-EX-BP-MAX                            GBIFPGM 
01948           VARYING GAA-INDEX                                       GBIFPGM 
01949             FROM 1 BY 1                                           GBIFPGM 
01950               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01951                                                                   GBIFPGM 
01952 *MQ 10/03                                                         GBIFPGM 
01953  4000-4396.                                                       GBIFPGM 
01954                                                                   GBIFPGM 
01955      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01956         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01957         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01958         PERFORM 4396-BLD-LENS-BP-MAX                              GBIFPGM 
01959           VARYING GAA-INDEX                                       GBIFPGM 
01960             FROM 1 BY 1                                           GBIFPGM 
01961               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01962      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01963         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01964         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01965         PERFORM 4396-BLD-LENS-BP-MAX                              GBIFPGM 
01966           VARYING GAA-INDEX                                       GBIFPGM 
01967             FROM 1 BY 1                                           GBIFPGM 
01968               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01969                                                                   GBIFPGM 
01970 *MQ 04/07/04                                                      GBIFPGM 
01971  4000-4398.                                                       GBIFPGM 
01972                                                                   GBIFPGM 
01973      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
01974         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01975         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01976         PERFORM 4398-BLD-VIS-HW-BP-MAX                            GBIFPGM 
01977           VARYING GAA-INDEX                                       GBIFPGM 
01978             FROM 1 BY 1                                           GBIFPGM 
01979               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01980      IF WS-CON-ABM-FOUND                                          GBIFPGM 
01981         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01982         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
01983         PERFORM 4398-BLD-VIS-HW-BP-MAX                            GBIFPGM 
01984           VARYING GAA-INDEX                                       GBIFPGM 
01985             FROM 1 BY 1                                           GBIFPGM 
01986               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
01987                                                                   GBIFPGM 
01988  4000-4500.                                                       GBIFPGM 
01989                                                                   GBIFPGM 
01990      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
01991         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01992         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
01993         PERFORM 4500-BLD-IP-BC-MSA-PMT-LVL                        GBIFPGM 
01994           VARYING GAB-INDEX                                       GBIFPGM 
01995             FROM 1 BY 1                                           GBIFPGM 
01996               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
01997      IF WS-CON-ACL-FOUND                                          GBIFPGM 
01998         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
01999         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02000         PERFORM 4500-BLD-IP-BC-MSA-PMT-LVL                        GBIFPGM 
02001           VARYING GAB-INDEX                                       GBIFPGM 
02002             FROM 1 BY 1                                           GBIFPGM 
02003               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02004                                                                   GBIFPGM 
02005  4000-4510.                                                       GBIFPGM 
02006                                                                   GBIFPGM 
02007      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02008         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02009         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02010         PERFORM 4510-BLD-IP-BS-MSA-PMT-LVL                        GBIFPGM 
02011           VARYING GAB-INDEX                                       GBIFPGM 
02012             FROM 1 BY 1                                           GBIFPGM 
02013               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02014      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02015         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02016         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02017         PERFORM 4510-BLD-IP-BS-MSA-PMT-LVL                        GBIFPGM 
02018           VARYING GAB-INDEX                                       GBIFPGM 
02019             FROM 1 BY 1                                           GBIFPGM 
02020               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02021                                                                   GBIFPGM 
02022  4000-4520.                                                       GBIFPGM 
02023                                                                   GBIFPGM 
02024      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02025         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02026         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02027         PERFORM 4520-BLD-IP-BCBS-MSA-PMT-LVL                      GBIFPGM 
02028           VARYING GAB-INDEX                                       GBIFPGM 
02029             FROM 1 BY 1                                           GBIFPGM 
02030               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02031      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02032         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02033         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02034         PERFORM 4520-BLD-IP-BCBS-MSA-PMT-LVL                      GBIFPGM 
02035           VARYING GAB-INDEX                                       GBIFPGM 
02036             FROM 1 BY 1                                           GBIFPGM 
02037               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02038                                                                   GBIFPGM 
02039  4000-4530.                                                       GBIFPGM 
02040                                                                   GBIFPGM 
02041      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02042         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02043         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02044         PERFORM 4530-BLD-IP-BC-MSA-BP-MAX                         GBIFPGM 
02045           VARYING GAA-INDEX                                       GBIFPGM 
02046             FROM 1 BY 1                                           GBIFPGM 
02047               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02048      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02049         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02050         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02051         PERFORM 4530-BLD-IP-BC-MSA-BP-MAX                         GBIFPGM 
02052           VARYING GAA-INDEX                                       GBIFPGM 
02053             FROM 1 BY 1                                           GBIFPGM 
02054               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02055                                                                   GBIFPGM 
02056  4000-4540.                                                       GBIFPGM 
02057                                                                   GBIFPGM 
02058      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02059         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02060         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02061         PERFORM 4540-BLD-IP-BS-MSA-BP-MAX                         GBIFPGM 
02062           VARYING GAA-INDEX                                       GBIFPGM 
02063             FROM 1 BY 1                                           GBIFPGM 
02064               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02065      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02066         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02067         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02068         PERFORM 4540-BLD-IP-BS-MSA-BP-MAX                         GBIFPGM 
02069           VARYING GAA-INDEX                                       GBIFPGM 
02070             FROM 1 BY 1                                           GBIFPGM 
02071               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02072                                                                   GBIFPGM 
02073  4000-4550.                                                       GBIFPGM 
02074                                                                   GBIFPGM 
02075      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02076         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02077         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02078         PERFORM 4550-BLD-IP-BCBS-BP-MAX                           GBIFPGM 
02079           VARYING GAA-INDEX                                       GBIFPGM 
02080             FROM 1 BY 1                                           GBIFPGM 
02081               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02082      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02083         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02084         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02085         PERFORM 4550-BLD-IP-BCBS-BP-MAX                           GBIFPGM 
02086           VARYING GAA-INDEX                                       GBIFPGM 
02087             FROM 1 BY 1                                           GBIFPGM 
02088               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02089                                                                   GBIFPGM 
02090  4000-4560.                                                       GBIFPGM 
02091                                                                   GBIFPGM 
02092      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02093         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02094         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02095         PERFORM 4560-BLD-IP-BC-MSA-LIFE-MAX                       GBIFPGM 
02096           VARYING GAA-INDEX                                       GBIFPGM 
02097             FROM 1 BY 1                                           GBIFPGM 
02098               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02099      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02100         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02101         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02102         PERFORM 4560-BLD-IP-BC-MSA-LIFE-MAX                       GBIFPGM 
02103           VARYING GAA-INDEX                                       GBIFPGM 
02104             FROM 1 BY 1                                           GBIFPGM 
02105               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02106                                                                   GBIFPGM 
02107  4000-4570.                                                       GBIFPGM 
02108                                                                   GBIFPGM 
02109      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02110         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02111         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02112         PERFORM 4570-BLD-IP-BS-MSA-LIFE-MAX                       GBIFPGM 
02113           VARYING GAA-INDEX                                       GBIFPGM 
02114             FROM 1 BY 1                                           GBIFPGM 
02115               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02116      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02117         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02118         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02119         PERFORM 4570-BLD-IP-BS-MSA-LIFE-MAX                       GBIFPGM 
02120           VARYING GAA-INDEX                                       GBIFPGM 
02121             FROM 1 BY 1                                           GBIFPGM 
02122               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02123                                                                   GBIFPGM 
02124  4000-4580.                                                       GBIFPGM 
02125                                                                   GBIFPGM 
02126      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02127         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02128         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02129         PERFORM 4580-BLD-IP-BCBS-MSA-LIFE-MAX                     GBIFPGM 
02130           VARYING GAA-INDEX                                       GBIFPGM 
02131             FROM 1 BY 1                                           GBIFPGM 
02132               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02133      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02134         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02135         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02136         PERFORM 4580-BLD-IP-BCBS-MSA-LIFE-MAX                     GBIFPGM 
02137           VARYING GAA-INDEX                                       GBIFPGM 
02138             FROM 1 BY 1                                           GBIFPGM 
02139               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02140                                                                   GBIFPGM 
02141  4000-4590.                                                       GBIFPGM 
02142                                                                   GBIFPGM 
02143      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02144         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02145         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02146         PERFORM 4590-BLD-IP-BC-MH-PMT-LVL                         GBIFPGM 
02147           VARYING GAB-INDEX                                       GBIFPGM 
02148             FROM 1 BY 1                                           GBIFPGM 
02149               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02150      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02151         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02152         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02153         PERFORM 4590-BLD-IP-BC-MH-PMT-LVL                         GBIFPGM 
02154           VARYING GAB-INDEX                                       GBIFPGM 
02155             FROM 1 BY 1                                           GBIFPGM 
02156               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02157                                                                   GBIFPGM 
02158  4000-4600.                                                       GBIFPGM 
02159                                                                   GBIFPGM 
02160      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02161         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02162         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02163         PERFORM 4600-BLD-IP-BS-MH-PMT-LVL                         GBIFPGM 
02164           VARYING GAB-INDEX                                       GBIFPGM 
02165             FROM 1 BY 1                                           GBIFPGM 
02166               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02167      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02168         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02169         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02170         PERFORM 4600-BLD-IP-BS-MH-PMT-LVL                         GBIFPGM 
02171           VARYING GAB-INDEX                                       GBIFPGM 
02172             FROM 1 BY 1                                           GBIFPGM 
02173               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02174                                                                   GBIFPGM 
02175  4000-4610.                                                       GBIFPGM 
02176                                                                   GBIFPGM 
02177      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02178         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02179         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02180         PERFORM 4610-BLD-IP-BCBS-MH-PMT-LVL                       GBIFPGM 
02181           VARYING GAB-INDEX                                       GBIFPGM 
02182             FROM 1 BY 1                                           GBIFPGM 
02183               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02184      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02185         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02186         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02187         PERFORM 4610-BLD-IP-BCBS-MH-PMT-LVL                       GBIFPGM 
02188           VARYING GAB-INDEX                                       GBIFPGM 
02189             FROM 1 BY 1                                           GBIFPGM 
02190               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02191                                                                   GBIFPGM 
02192  4000-4620.                                                       GBIFPGM 
02193                                                                   GBIFPGM 
02194      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02195         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02196         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02197         PERFORM 4620-BLD-IP-BC-MH-BP-MAX                          GBIFPGM 
02198           VARYING GAA-INDEX                                       GBIFPGM 
02199             FROM 1 BY 1                                           GBIFPGM 
02200               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02201      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02202         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02203         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02204         PERFORM 4620-BLD-IP-BC-MH-BP-MAX                          GBIFPGM 
02205           VARYING GAA-INDEX                                       GBIFPGM 
02206             FROM 1 BY 1                                           GBIFPGM 
02207               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02208                                                                   GBIFPGM 
02690  4000-4625.                                                       GBIFPGM 
02691                                                                   GBIFPGM 
02692      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02693         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02694         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02695         PERFORM 4625-BLD-BC-MH-BP-MAX-POT                         GBIFPGM 
02696           VARYING GAA-INDEX                                       GBIFPGM 
02697             FROM 1 BY 1                                           GBIFPGM 
02698               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02699      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02700         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02701         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02702         PERFORM 4625-BLD-BC-MH-BP-MAX-POT                         GBIFPGM 
02703           VARYING GAA-INDEX                                       GBIFPGM 
02704             FROM 1 BY 1                                           GBIFPGM 
02705               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02706                                                                   GBIFPGM 
02209  4000-4630.                                                       GBIFPGM 
02210                                                                   GBIFPGM 
02211      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02212         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02213         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02214         PERFORM 4630-BLD-IP-BS-MH-BP-MAX                          GBIFPGM 
02215           VARYING GAA-INDEX                                       GBIFPGM 
02216             FROM 1 BY 1                                           GBIFPGM 
02217               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02218      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02219         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02220         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02221         PERFORM 4630-BLD-IP-BS-MH-BP-MAX                          GBIFPGM 
02222           VARYING GAA-INDEX                                       GBIFPGM 
02223             FROM 1 BY 1                                           GBIFPGM 
02224               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02225                                                                   GBIFPGM 
02724  4000-4635.                                                       GBIFPGM 
02725                                                                   GBIFPGM 
02726      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02727         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02728         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02729         PERFORM 4635-BLD-BS-MH-BP-MAX-POT                         GBIFPGM 
02730           VARYING GAA-INDEX                                       GBIFPGM 
02731             FROM 1 BY 1                                           GBIFPGM 
02732               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02733      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02734         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02735         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02736         PERFORM 4635-BLD-BS-MH-BP-MAX-POT                         GBIFPGM 
02737           VARYING GAA-INDEX                                       GBIFPGM 
02738             FROM 1 BY 1                                           GBIFPGM 
02739               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02740                                                                   GBIFPGM 
02226  4000-4640.                                                       GBIFPGM 
02227                                                                   GBIFPGM 
02228      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02229         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02230         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02231         PERFORM 4640-BLD-IP-BCBS-MH-BP-MAX                        GBIFPGM 
02232           VARYING GAA-INDEX                                       GBIFPGM 
02233             FROM 1 BY 1                                           GBIFPGM 
02234               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02235      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02236         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02237         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02238         PERFORM 4640-BLD-IP-BCBS-MH-BP-MAX                        GBIFPGM 
02239           VARYING GAA-INDEX                                       GBIFPGM 
02240             FROM 1 BY 1                                           GBIFPGM 
02241               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02242                                                                   GBIFPGM 
02758  4000-4645.                                                       GBIFPGM 
02759                                                                   GBIFPGM 
02760      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02761         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02762         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02763         PERFORM 4645-BLD-BCBS-MH-BP-MAX-POT                       GBIFPGM 
02764           VARYING GAA-INDEX                                       GBIFPGM 
02765             FROM 1 BY 1                                           GBIFPGM 
02766               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02767      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02768         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02769         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02770         PERFORM 4645-BLD-BCBS-MH-BP-MAX-POT                       GBIFPGM 
02771           VARYING GAA-INDEX                                       GBIFPGM 
02772             FROM 1 BY 1                                           GBIFPGM 
02773               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02774                                                                   GBIFPGM 
02243  4000-4650.                                                       GBIFPGM 
02244                                                                   GBIFPGM 
02245      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02246         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02247         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02248         PERFORM 4650-BLD-IP-BC-MH-LIFE-MAX                        GBIFPGM 
02249           VARYING GAA-INDEX                                       GBIFPGM 
02250             FROM 1 BY 1                                           GBIFPGM 
02251               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02252      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02253         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02254         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02255         PERFORM 4650-BLD-IP-BC-MH-LIFE-MAX                        GBIFPGM 
02256           VARYING GAA-INDEX                                       GBIFPGM 
02257             FROM 1 BY 1                                           GBIFPGM 
02258               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02259                                                                   GBIFPGM 
02792  4000-4655.                                                       GBIFPGM 
02793                                                                   GBIFPGM 
02794      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02795         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02796         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02797         PERFORM 4655-BLD-BC-MH-LIFE-MAX-POT                       GBIFPGM 
02798           VARYING GAA-INDEX                                       GBIFPGM 
02799             FROM 1 BY 1                                           GBIFPGM 
02800               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02801      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02802         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02803         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02804         PERFORM 4655-BLD-BC-MH-LIFE-MAX-POT                       GBIFPGM 
02805           VARYING GAA-INDEX                                       GBIFPGM 
02806             FROM 1 BY 1                                           GBIFPGM 
02807               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02808                                                                   GBIFPGM 
02260  4000-4660.                                                       GBIFPGM 
02261                                                                   GBIFPGM 
02262      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02263         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02264         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02265         PERFORM 4660-BLD-IP-BS-MH-LIFE-MAX                        GBIFPGM 
02266           VARYING GAA-INDEX                                       GBIFPGM 
02267             FROM 1 BY 1                                           GBIFPGM 
02268               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02269      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02270         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02271         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02272         PERFORM 4660-BLD-IP-BS-MH-LIFE-MAX                        GBIFPGM 
02273           VARYING GAA-INDEX                                       GBIFPGM 
02274             FROM 1 BY 1                                           GBIFPGM 
02275               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02276                                                                   GBIFPGM 
02826  4000-4665.                                                       GBIFPGM 
02827                                                                   GBIFPGM 
02828      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02829         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02830         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02831         PERFORM 4665-BLD-BS-MH-LIFE-MAX-POT                       GBIFPGM 
02832           VARYING GAA-INDEX                                       GBIFPGM 
02833             FROM 1 BY 1                                           GBIFPGM 
02834               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02835      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02836         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02837         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02838         PERFORM 4665-BLD-BS-MH-LIFE-MAX-POT                       GBIFPGM 
02839           VARYING GAA-INDEX                                       GBIFPGM 
02840             FROM 1 BY 1                                           GBIFPGM 
02841               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02842                                                                   GBIFPGM 
02277  4000-4670.                                                       GBIFPGM 
02278                                                                   GBIFPGM 
02279      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02280         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02281         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02282         PERFORM 4670-BLD-IP-BCBS-MH-LIFE-MAX                      GBIFPGM 
02283           VARYING GAA-INDEX                                       GBIFPGM 
02284             FROM 1 BY 1                                           GBIFPGM 
02285               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02286      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02287         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02288         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02289         PERFORM 4670-BLD-IP-BCBS-MH-LIFE-MAX                      GBIFPGM 
02856           VARYING GAA-INDEX                                       GBIFPGM 
02857             FROM 1 BY 1                                           GBIFPGM 
02858               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02859                                                                   GBIFPGM 
02860  4000-4675.                                                       GBIFPGM 
02861                                                                   GBIFPGM 
02862      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02863         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02864         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02865         PERFORM 4675-BLD-BCBS-MH-LIFE-MAX-POT                     GBIFPGM 
02866           VARYING GAA-INDEX                                       GBIFPGM 
02867             FROM 1 BY 1                                           GBIFPGM 
02868               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02869      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02870         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02871         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02872         PERFORM 4675-BLD-BCBS-MH-LIFE-MAX-POT                     GBIFPGM 
02290           VARYING GAA-INDEX                                       GBIFPGM 
02291             FROM 1 BY 1                                           GBIFPGM 
02292               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02293                                                                   GBIFPGM 
02294  4000-4680.                                                       GBIFPGM 
02295                                                                   GBIFPGM 
02296      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02297         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02298         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02299         PERFORM 4680-BLD-IP-BC-SA-PMT-LVL                         GBIFPGM 
02300           VARYING GAB-INDEX                                       GBIFPGM 
02301             FROM 1 BY 1                                           GBIFPGM 
02302               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02303      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02304         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02305         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02306         PERFORM 4680-BLD-IP-BC-SA-PMT-LVL                         GBIFPGM 
02307           VARYING GAB-INDEX                                       GBIFPGM 
02308             FROM 1 BY 1                                           GBIFPGM 
02309               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02310                                                                   GBIFPGM 
02311  4000-4690.                                                       GBIFPGM 
02312                                                                   GBIFPGM 
02313      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02314         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02315         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02316         PERFORM 4690-BLD-IP-BS-SA-PMT-LVL                         GBIFPGM 
02317           VARYING GAB-INDEX                                       GBIFPGM 
02318             FROM 1 BY 1                                           GBIFPGM 
02319               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02320      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02321         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02322         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02323         PERFORM 4690-BLD-IP-BS-SA-PMT-LVL                         GBIFPGM 
02324           VARYING GAB-INDEX                                       GBIFPGM 
02325             FROM 1 BY 1                                           GBIFPGM 
02326               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02327                                                                   GBIFPGM 
02328  4000-4700.                                                       GBIFPGM 
02329                                                                   GBIFPGM 
02330      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02331         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02332         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02333         PERFORM 4700-BLD-IP-BCBS-SA-PMT-LVL                       GBIFPGM 
02334           VARYING GAB-INDEX                                       GBIFPGM 
02335             FROM 1 BY 1                                           GBIFPGM 
02336               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02337      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02338         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02339         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02340         PERFORM 4700-BLD-IP-BCBS-SA-PMT-LVL                       GBIFPGM 
02341           VARYING GAB-INDEX                                       GBIFPGM 
02342             FROM 1 BY 1                                           GBIFPGM 
02343               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02344                                                                   GBIFPGM 
02345  4000-4710.                                                       GBIFPGM 
02346                                                                   GBIFPGM 
02347      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02348         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02349         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02350         PERFORM 4710-BLD-IP-BC-SA-BEN-PERD-MAX                    GBIFPGM 
02351           VARYING GAA-INDEX                                       GBIFPGM 
02352             FROM 1 BY 1                                           GBIFPGM 
02353               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02354      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02355         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02356         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02357         PERFORM 4710-BLD-IP-BC-SA-BEN-PERD-MAX                    GBIFPGM 
02358           VARYING GAA-INDEX                                       GBIFPGM 
02359             FROM 1 BY 1                                           GBIFPGM 
02360               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02361                                                                   GBIFPGM 
02362 *MQ 10/03                                                         GBIFPGM 
02363  4000-4715.                                                       GBIFPGM 
02364                                                                   GBIFPGM 
02365      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02366         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02367         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02368         PERFORM 4715-BLD-BC-SA-BP-MAX-POT                         GBIFPGM 
02369           VARYING GAA-INDEX                                       GBIFPGM 
02370             FROM 1 BY 1                                           GBIFPGM 
02371               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02372      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02373         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02374         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02375         PERFORM 4715-BLD-BC-SA-BP-MAX-POT                         GBIFPGM 
02376           VARYING GAA-INDEX                                       GBIFPGM 
02377             FROM 1 BY 1                                           GBIFPGM 
02378               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02379                                                                   GBIFPGM 
02380  4000-4720.                                                       GBIFPGM 
02381                                                                   GBIFPGM 
02382      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02383         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02384         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02385         PERFORM 4720-BLD-IP-BS-SA-BEN-PERD-MAX                    GBIFPGM 
02386           VARYING GAA-INDEX                                       GBIFPGM 
02387             FROM 1 BY 1                                           GBIFPGM 
02388               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02389      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02390         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02391         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02392         PERFORM 4720-BLD-IP-BS-SA-BEN-PERD-MAX                    GBIFPGM 
02393           VARYING GAA-INDEX                                       GBIFPGM 
02394             FROM 1 BY 1                                           GBIFPGM 
02395               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02396                                                                   GBIFPGM 
02397 *MQ 10/03                                                         GBIFPGM 
02398  4000-4725.                                                       GBIFPGM 
02399                                                                   GBIFPGM 
02400      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02401         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02402         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02403         PERFORM 4725-BLD-BS-SA-BP-MAX-POT                         GBIFPGM 
02404           VARYING GAA-INDEX                                       GBIFPGM 
02405             FROM 1 BY 1                                           GBIFPGM 
02406               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02407      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02408         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02409         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02410         PERFORM 4725-BLD-BS-SA-BP-MAX-POT                         GBIFPGM 
02411           VARYING GAA-INDEX                                       GBIFPGM 
02412             FROM 1 BY 1                                           GBIFPGM 
02413               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02414                                                                   GBIFPGM 
02415  4000-4730.                                                       GBIFPGM 
02416                                                                   GBIFPGM 
02417      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02418         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02419         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02420         PERFORM 4730-BLD-IP-BCBS-SA-BP-MAX                        GBIFPGM 
02421           VARYING GAA-INDEX                                       GBIFPGM 
02422             FROM 1 BY 1                                           GBIFPGM 
02423               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02424      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02425         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02426         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02427         PERFORM 4730-BLD-IP-BCBS-SA-BP-MAX                        GBIFPGM 
02428           VARYING GAA-INDEX                                       GBIFPGM 
02429             FROM 1 BY 1                                           GBIFPGM 
02430               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02431                                                                   GBIFPGM 
02432 *MQ 10/03                                                         GBIFPGM 
02433  4000-4735.                                                       GBIFPGM 
02434                                                                   GBIFPGM 
02435      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02436         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02437         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02438         PERFORM 4735-BLD-BCBS-SA-BP-MAX-POT                       GBIFPGM 
02439           VARYING GAA-INDEX                                       GBIFPGM 
02440             FROM 1 BY 1                                           GBIFPGM 
02441               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02442      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02443         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02444         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02445         PERFORM 4735-BLD-BCBS-SA-BP-MAX-POT                       GBIFPGM 
02446           VARYING GAA-INDEX                                       GBIFPGM 
02447             FROM 1 BY 1                                           GBIFPGM 
02448               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02449                                                                   GBIFPGM 
02450  4000-4740.                                                       GBIFPGM 
02451                                                                   GBIFPGM 
02452      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02453         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02454         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02455         PERFORM 4740-BLD-IP-BC-SA-LIFE-MAX                        GBIFPGM 
02456           VARYING GAA-INDEX                                       GBIFPGM 
02457             FROM 1 BY 1                                           GBIFPGM 
02458               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02459      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02460         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02461         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02462         PERFORM 4740-BLD-IP-BC-SA-LIFE-MAX                        GBIFPGM 
02463           VARYING GAA-INDEX                                       GBIFPGM 
02464             FROM 1 BY 1                                           GBIFPGM 
02465               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02466                                                                   GBIFPGM 
02467 *MQ 10/03                                                         GBIFPGM 
02468  4000-4745.                                                       GBIFPGM 
02469                                                                   GBIFPGM 
02470      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02471         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02472         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02473         PERFORM 4745-BLD-BC-SA-LIFE-MAX-POT                       GBIFPGM 
02474           VARYING GAA-INDEX                                       GBIFPGM 
02475             FROM 1 BY 1                                           GBIFPGM 
02476               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02477      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02478         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02479         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02480         PERFORM 4745-BLD-BC-SA-LIFE-MAX-POT                       GBIFPGM 
02481           VARYING GAA-INDEX                                       GBIFPGM 
02482             FROM 1 BY 1                                           GBIFPGM 
02483               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02484                                                                   GBIFPGM 
02485  4000-4750.                                                       GBIFPGM 
02486                                                                   GBIFPGM 
02487      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02488         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02489         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02490         PERFORM 4750-BLD-IP-BS-SA-LIFE-MAX                        GBIFPGM 
02491           VARYING GAA-INDEX                                       GBIFPGM 
02492             FROM 1 BY 1                                           GBIFPGM 
02493               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02494      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02495         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02496         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02497         PERFORM 4750-BLD-IP-BS-SA-LIFE-MAX                        GBIFPGM 
02498           VARYING GAA-INDEX                                       GBIFPGM 
02499             FROM 1 BY 1                                           GBIFPGM 
02500               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02501                                                                   GBIFPGM 
02502 *MQ 10/03                                                         GBIFPGM 
02503  4000-4755.                                                       GBIFPGM 
02504                                                                   GBIFPGM 
02505      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02506         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02507         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02508         PERFORM 4755-BLD-BS-SA-LIFE-MAX-POT                       GBIFPGM 
02509           VARYING GAA-INDEX                                       GBIFPGM 
02510             FROM 1 BY 1                                           GBIFPGM 
02511               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02512      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02513         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02514         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02515         PERFORM 4755-BLD-BS-SA-LIFE-MAX-POT                       GBIFPGM 
02516           VARYING GAA-INDEX                                       GBIFPGM 
02517             FROM 1 BY 1                                           GBIFPGM 
02518               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02519                                                                   GBIFPGM 
02520  4000-4760.                                                       GBIFPGM 
02521                                                                   GBIFPGM 
02522      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02523         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02524         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02525         PERFORM 4760-BLD-IP-BCBS-SA-LIFE-MAX                      GBIFPGM 
02526           VARYING GAA-INDEX                                       GBIFPGM 
02527             FROM 1 BY 1                                           GBIFPGM 
02528               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02529      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02530         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02531         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02532         PERFORM 4760-BLD-IP-BCBS-SA-LIFE-MAX                      GBIFPGM 
02533           VARYING GAA-INDEX                                       GBIFPGM 
02534             FROM 1 BY 1                                           GBIFPGM 
02535               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02536                                                                   GBIFPGM 
02537 *MQ 10/03                                                         GBIFPGM 
02538  4000-4762.                                                       GBIFPGM 
02539                                                                   GBIFPGM 
02540      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02541         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02542         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02543         PERFORM 4762-BLD-SA-LIFE-CONF-MAX                         GBIFPGM 
02544           VARYING GAA-INDEX                                       GBIFPGM 
02545             FROM 1 BY 1                                           GBIFPGM 
02546               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02547      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02548         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02549         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02550         PERFORM 4762-BLD-SA-LIFE-CONF-MAX                         GBIFPGM 
02551           VARYING GAA-INDEX                                       GBIFPGM 
02552             FROM 1 BY 1                                           GBIFPGM 
02553               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02554 *MQ 10/03                                                         GBIFPGM 
02555  4000-4765.                                                       GBIFPGM 
02556                                                                   GBIFPGM 
02557      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02558         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02559         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02560         PERFORM 4765-BLD-BCBS-SA-LIFE-MAX-POT                     GBIFPGM 
02561           VARYING GAA-INDEX                                       GBIFPGM 
02562             FROM 1 BY 1                                           GBIFPGM 
02563               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02564      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02565         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02566         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02567         PERFORM 4765-BLD-BCBS-SA-LIFE-MAX-POT                     GBIFPGM 
02568           VARYING GAA-INDEX                                       GBIFPGM 
02569             FROM 1 BY 1                                           GBIFPGM 
02570               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02571                                                                   GBIFPGM 
02572  4000-4770.                                                       GBIFPGM 
02573                                                                   GBIFPGM 
02574      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02575         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02576         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02577         PERFORM 4770-BLD-OP-BC-MSA-PMT-LVL                        GBIFPGM 
02578           VARYING GAB-INDEX                                       GBIFPGM 
02579             FROM 1 BY 1                                           GBIFPGM 
02580               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02581      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02582         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02583         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02584         PERFORM 4770-BLD-OP-BC-MSA-PMT-LVL                        GBIFPGM 
02585           VARYING GAB-INDEX                                       GBIFPGM 
02586             FROM 1 BY 1                                           GBIFPGM 
02587               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02588                                                                   GBIFPGM 
02589 *MQ 10/03                                                         GBIFPGM 
02590  4000-4775.                                                       GBIFPGM 
02591                                                                   GBIFPGM 
02592      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02593         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02594         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02595         PERFORM 4775-BLD-BC-MSA-PMT-LVL-POT                       GBIFPGM 
02596           VARYING GAB-INDEX                                       GBIFPGM 
02597             FROM 1 BY 1                                           GBIFPGM 
02598               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02599      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02600         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02601         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02602         PERFORM 4775-BLD-BC-MSA-PMT-LVL-POT                       GBIFPGM 
02603           VARYING GAB-INDEX                                       GBIFPGM 
02604             FROM 1 BY 1                                           GBIFPGM 
02605               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02606                                                                   GBIFPGM 
02607  4000-4780.                                                       GBIFPGM 
02608                                                                   GBIFPGM 
02609      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02610         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02611         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02612         PERFORM 4780-BLD-OP-BS-MSA-PMT-LVL                        GBIFPGM 
02613           VARYING GAB-INDEX                                       GBIFPGM 
02614             FROM 1 BY 1                                           GBIFPGM 
02615               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02616      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02617         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02618         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02619         PERFORM 4780-BLD-OP-BS-MSA-PMT-LVL                        GBIFPGM 
02620           VARYING GAB-INDEX                                       GBIFPGM 
02621             FROM 1 BY 1                                           GBIFPGM 
02622               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02623                                                                   GBIFPGM 
02624 *MQ 10/03                                                         GBIFPGM 
02625  4000-4785.                                                       GBIFPGM 
02626                                                                   GBIFPGM 
02627      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02628         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02629         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02630         PERFORM 4785-BLD-BS-MSA-PMT-LVL-POT                       GBIFPGM 
02631           VARYING GAB-INDEX                                       GBIFPGM 
02632             FROM 1 BY 1                                           GBIFPGM 
02633               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02634      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02635         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02636         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02637         PERFORM 4785-BLD-BS-MSA-PMT-LVL-POT                       GBIFPGM 
02638           VARYING GAB-INDEX                                       GBIFPGM 
02639             FROM 1 BY 1                                           GBIFPGM 
02640               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02641                                                                   GBIFPGM 
02642  4000-4790.                                                       GBIFPGM 
02643                                                                   GBIFPGM 
02644      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02645         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02646         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02647         PERFORM 4790-BLD-OP-BCBS-MSA-PMT-LVL                      GBIFPGM 
02648           VARYING GAB-INDEX                                       GBIFPGM 
02649             FROM 1 BY 1                                           GBIFPGM 
02650               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02651      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02652         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02653         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02654         PERFORM 4790-BLD-OP-BCBS-MSA-PMT-LVL                      GBIFPGM 
02655           VARYING GAB-INDEX                                       GBIFPGM 
02656             FROM 1 BY 1                                           GBIFPGM 
02657               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02658                                                                   GBIFPGM 
02659 *MQ 10/03                                                         GBIFPGM 
02660  4000-4795.                                                       GBIFPGM 
02661                                                                   GBIFPGM 
02662      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02663         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02664         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02665         PERFORM 4795-BLD-BCBS-MSA-PMT-LVL-POT                     GBIFPGM 
02666           VARYING GAB-INDEX                                       GBIFPGM 
02667             FROM 1 BY 1                                           GBIFPGM 
02668               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02669      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02670         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02671         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02672         PERFORM 4795-BLD-BCBS-MSA-PMT-LVL-POT                     GBIFPGM 
02673           VARYING GAB-INDEX                                       GBIFPGM 
02674             FROM 1 BY 1                                           GBIFPGM 
02675               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02676                                                                   GBIFPGM 
02677  4000-4800.                                                       GBIFPGM 
02678                                                                   GBIFPGM 
02679      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02680         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02681         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02682         PERFORM 4800-BLD-OP-BC-MSA-BP-MAX                         GBIFPGM 
02683           VARYING GAA-INDEX                                       GBIFPGM 
02684             FROM 1 BY 1                                           GBIFPGM 
02685               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02686      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02687         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02688         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02689         PERFORM 4800-BLD-OP-BC-MSA-BP-MAX                         GBIFPGM 
02690           VARYING GAA-INDEX                                       GBIFPGM 
02691             FROM 1 BY 1                                           GBIFPGM 
02692               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02693                                                                   GBIFPGM 
02694  4000-4810.                                                       GBIFPGM 
02695                                                                   GBIFPGM 
02696      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02697         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02698         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02699         PERFORM 4810-BLD-OP-BS-MSA-BP-MAX                         GBIFPGM 
02700           VARYING GAA-INDEX                                       GBIFPGM 
02701             FROM 1 BY 1                                           GBIFPGM 
02702               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02703      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02704         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02705         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02706         PERFORM 4810-BLD-OP-BS-MSA-BP-MAX                         GBIFPGM 
02707           VARYING GAA-INDEX                                       GBIFPGM 
02708             FROM 1 BY 1                                           GBIFPGM 
02709               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02710                                                                   GBIFPGM 
02711  4000-4820.                                                       GBIFPGM 
02712                                                                   GBIFPGM 
02713      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02714         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02715         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02716         PERFORM 4820-BLD-OP-BCBS-MSA-BP-MAX                       GBIFPGM 
02717           VARYING GAA-INDEX                                       GBIFPGM 
02718             FROM 1 BY 1                                           GBIFPGM 
02719               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02720      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02721         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02722         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02723         PERFORM 4820-BLD-OP-BCBS-MSA-BP-MAX                       GBIFPGM 
02724           VARYING GAA-INDEX                                       GBIFPGM 
02725             FROM 1 BY 1                                           GBIFPGM 
02726               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02727                                                                   GBIFPGM 
02728 *MQ 10/03                                                         GBIFPGM 
02729  4000-4822.                                                       GBIFPGM 
02730                                                                   GBIFPGM 
02731      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02732         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02733         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02734         PERFORM 4822-BLD-BCBS-OP-MSA-BP-MAX-CH                    GBIFPGM 
02735           VARYING GAA-INDEX                                       GBIFPGM 
02736             FROM 1 BY 1                                           GBIFPGM 
02737               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02738      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02739         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02740         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02741         PERFORM 4822-BLD-BCBS-OP-MSA-BP-MAX-CH                    GBIFPGM 
02742           VARYING GAA-INDEX                                       GBIFPGM 
02743             FROM 1 BY 1                                           GBIFPGM 
02744               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02745                                                                   GBIFPGM 
02746 *MQ 10/03                                                         GBIFPGM 
02747  4000-4825.                                                       GBIFPGM 
02748                                                                   GBIFPGM 
02749      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02750         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02751         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02752         PERFORM 4825-BLD-BCBS-OP-MSA-BP-MAX-AD                    GBIFPGM 
02753           VARYING GAA-INDEX                                       GBIFPGM 
02754             FROM 1 BY 1                                           GBIFPGM 
02755               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02756      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02757         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02758         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02759         PERFORM 4825-BLD-BCBS-OP-MSA-BP-MAX-AD                    GBIFPGM 
02760           VARYING GAA-INDEX                                       GBIFPGM 
02761             FROM 1 BY 1                                           GBIFPGM 
02762               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02763                                                                   GBIFPGM 
02764  4000-4830.                                                       GBIFPGM 
02765                                                                   GBIFPGM 
02766      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02767         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02768         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02769         PERFORM 4830-BLD-OP-BC-MSA-LIFE-MAX                       GBIFPGM 
02770           VARYING GAA-INDEX                                       GBIFPGM 
02771             FROM 1 BY 1                                           GBIFPGM 
02772               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02773      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02774         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02775         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02776         PERFORM 4830-BLD-OP-BC-MSA-LIFE-MAX                       GBIFPGM 
02777           VARYING GAA-INDEX                                       GBIFPGM 
02778             FROM 1 BY 1                                           GBIFPGM 
02779               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02780                                                                   GBIFPGM 
02781  4000-4840.                                                       GBIFPGM 
02782                                                                   GBIFPGM 
02783      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02784         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02785         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02786         PERFORM 4840-BLD-OP-BS-MSA-LIFE-MAX                       GBIFPGM 
02787           VARYING GAA-INDEX                                       GBIFPGM 
02788             FROM 1 BY 1                                           GBIFPGM 
02789               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02790      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02791         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02792         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02793         PERFORM 4840-BLD-OP-BS-MSA-LIFE-MAX                       GBIFPGM 
02794           VARYING GAA-INDEX                                       GBIFPGM 
02795             FROM 1 BY 1                                           GBIFPGM 
02796               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02797                                                                   GBIFPGM 
02798  4000-4850.                                                       GBIFPGM 
02799                                                                   GBIFPGM 
02800      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02801         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02802         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02803         PERFORM 4850-BLD-OP-BCBS-MSA-LIFE-MAX                     GBIFPGM 
02804           VARYING GAA-INDEX                                       GBIFPGM 
02805             FROM 1 BY 1                                           GBIFPGM 
02806               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02807      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02808         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02809         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02810         PERFORM 4850-BLD-OP-BCBS-MSA-LIFE-MAX                     GBIFPGM 
02811           VARYING GAA-INDEX                                       GBIFPGM 
02812             FROM 1 BY 1                                           GBIFPGM 
02813               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02814                                                                   GBIFPGM 
02815  4000-4860.                                                       GBIFPGM 
02816                                                                   GBIFPGM 
02817      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02818         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02819         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02820         PERFORM 4860-BLD-OP-BC-MH-PMT-LVL                         GBIFPGM 
02821           VARYING GAB-INDEX                                       GBIFPGM 
02822             FROM 1 BY 1                                           GBIFPGM 
02823               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02824      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02825         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02826         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02827         PERFORM 4860-BLD-OP-BC-MH-PMT-LVL                         GBIFPGM 
02828           VARYING GAB-INDEX                                       GBIFPGM 
02829             FROM 1 BY 1                                           GBIFPGM 
02830               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02831                                                                   GBIFPGM 
02832 *MQ 10/03                                                         GBIFPGM 
02833  4000-4865.                                                       GBIFPGM 
02834                                                                   GBIFPGM 
02835      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02836         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02837         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02838         PERFORM 4865-BLD-BC-MH-PMT-LVL-POT                        GBIFPGM 
02839           VARYING GAB-INDEX                                       GBIFPGM 
02840             FROM 1 BY 1                                           GBIFPGM 
02841               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02842      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02843         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02844         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02845         PERFORM 4865-BLD-BC-MH-PMT-LVL-POT                        GBIFPGM 
02846           VARYING GAB-INDEX                                       GBIFPGM 
02847             FROM 1 BY 1                                           GBIFPGM 
02848               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02849                                                                   GBIFPGM 
02850  4000-4870.                                                       GBIFPGM 
02851                                                                   GBIFPGM 
02852      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02853         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02854         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02855         PERFORM 4870-BLD-OP-BS-MH-PMT-LVL                         GBIFPGM 
02856           VARYING GAB-INDEX                                       GBIFPGM 
02857             FROM 1 BY 1                                           GBIFPGM 
02858               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02859      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02860         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02861         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02862         PERFORM 4870-BLD-OP-BS-MH-PMT-LVL                         GBIFPGM 
02863           VARYING GAB-INDEX                                       GBIFPGM 
02864             FROM 1 BY 1                                           GBIFPGM 
02865               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02866                                                                   GBIFPGM 
02867 *MQ 10/03                                                         GBIFPGM 
02868  4000-4875.                                                       GBIFPGM 
02869                                                                   GBIFPGM 
02870      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02871         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02872         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02873         PERFORM 4875-BLD-BS-MH-PMT-LVL-POT                        GBIFPGM 
02874           VARYING GAB-INDEX                                       GBIFPGM 
02875             FROM 1 BY 1                                           GBIFPGM 
02876               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02877      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02878         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02879         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02880         PERFORM 4875-BLD-BS-MH-PMT-LVL-POT                        GBIFPGM 
02881           VARYING GAB-INDEX                                       GBIFPGM 
02882             FROM 1 BY 1                                           GBIFPGM 
02883               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02884                                                                   GBIFPGM 
02885  4000-4880.                                                       GBIFPGM 
02886                                                                   GBIFPGM 
02887      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02888         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02889         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02890         PERFORM 4880-BLD-OP-BCBS-MH-PMT-LVL                       GBIFPGM 
02891           VARYING GAB-INDEX                                       GBIFPGM 
02892             FROM 1 BY 1                                           GBIFPGM 
02893               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02894      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02895         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02896         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02897         PERFORM 4880-BLD-OP-BCBS-MH-PMT-LVL                       GBIFPGM 
02898           VARYING GAB-INDEX                                       GBIFPGM 
02899             FROM 1 BY 1                                           GBIFPGM 
02900               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02901                                                                   GBIFPGM 
02902 *MQ 10/03                                                         GBIFPGM 
02903  4000-4885.                                                       GBIFPGM 
02904                                                                   GBIFPGM 
02905      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
02906         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02907         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02908         PERFORM 4885-BLD-BCBS-MH-PMT-LVL-POT                      GBIFPGM 
02909           VARYING GAB-INDEX                                       GBIFPGM 
02910             FROM 1 BY 1                                           GBIFPGM 
02911               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02912      IF WS-CON-ACL-FOUND                                          GBIFPGM 
02913         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02914         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
02915         PERFORM 4885-BLD-BCBS-MH-PMT-LVL-POT                      GBIFPGM 
02916           VARYING GAB-INDEX                                       GBIFPGM 
02917             FROM 1 BY 1                                           GBIFPGM 
02918               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
02919                                                                   GBIFPGM 
02920  4000-4890.                                                       GBIFPGM 
02921                                                                   GBIFPGM 
02922      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02923         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02924         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02925         PERFORM 4890-BLD-OP-BC-MH-BP-MAX                          GBIFPGM 
02926           VARYING GAA-INDEX                                       GBIFPGM 
02927             FROM 1 BY 1                                           GBIFPGM 
02928               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02929      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02930         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02931         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02932         PERFORM 4890-BLD-OP-BC-MH-BP-MAX                          GBIFPGM 
02933           VARYING GAA-INDEX                                       GBIFPGM 
02934             FROM 1 BY 1                                           GBIFPGM 
02935               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02936                                                                   GBIFPGM 
02937  4000-4900.                                                       GBIFPGM 
02938                                                                   GBIFPGM 
02939      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02940         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02941         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02942         PERFORM 4900-BLD-OP-BS-MH-BP-MAX                          GBIFPGM 
02943           VARYING GAA-INDEX                                       GBIFPGM 
02944             FROM 1 BY 1                                           GBIFPGM 
02945               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02946      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02947         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02948         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02949         PERFORM 4900-BLD-OP-BS-MH-BP-MAX                          GBIFPGM 
02950           VARYING GAA-INDEX                                       GBIFPGM 
02951             FROM 1 BY 1                                           GBIFPGM 
02952               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02953                                                                   GBIFPGM 
02954  4000-4910.                                                       GBIFPGM 
02955                                                                   GBIFPGM 
02956      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02957         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02958         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02959         PERFORM 4910-BLD-OP-BCBS-MH-BP-MAX                        GBIFPGM 
02960           VARYING GAA-INDEX                                       GBIFPGM 
02961             FROM 1 BY 1                                           GBIFPGM 
02962               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02963      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02964         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02965         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02966         PERFORM 4910-BLD-OP-BCBS-MH-BP-MAX                        GBIFPGM 
02967           VARYING GAA-INDEX                                       GBIFPGM 
02968             FROM 1 BY 1                                           GBIFPGM 
02969               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02970                                                                   GBIFPGM 
02971 *MQ 10/03                                                         GBIFPGM 
02972  4000-4911.                                                       GBIFPGM 
02973                                                                   GBIFPGM 
02974      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02975         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02976         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02977         PERFORM 4911-BLD-BCBS-OP-MH-BP-MAX-CH                     GBIFPGM 
02978           VARYING GAA-INDEX                                       GBIFPGM 
02979             FROM 1 BY 1                                           GBIFPGM 
02980               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02981      IF WS-CON-ABM-FOUND                                          GBIFPGM 
02982         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02983         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02984         PERFORM 4911-BLD-BCBS-OP-MH-BP-MAX-CH                     GBIFPGM 
02985           VARYING GAA-INDEX                                       GBIFPGM 
02986             FROM 1 BY 1                                           GBIFPGM 
02987               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02988                                                                   GBIFPGM 
02989 *MQ 10/03                                                         GBIFPGM 
02990  4000-4915.                                                       GBIFPGM 
02991                                                                   GBIFPGM 
02992      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
02993         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
02994         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
02995         PERFORM 4915-BLD-BCBS-OP-MH-BP-MAX-AD                     GBIFPGM 
02996           VARYING GAA-INDEX                                       GBIFPGM 
02997             FROM 1 BY 1                                           GBIFPGM 
02998               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
02999      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03000         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03001         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03002         PERFORM 4915-BLD-BCBS-OP-MH-BP-MAX-AD                     GBIFPGM 
03003           VARYING GAA-INDEX                                       GBIFPGM 
03004             FROM 1 BY 1                                           GBIFPGM 
03005               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03006                                                                   GBIFPGM 
03007  4000-4920.                                                       GBIFPGM 
03008                                                                   GBIFPGM 
03009      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03010         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03011         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03012         PERFORM 4920-BLD-OP-BC-MH-LIFE-MAX                        GBIFPGM 
03013           VARYING GAA-INDEX                                       GBIFPGM 
03014             FROM 1 BY 1                                           GBIFPGM 
03015               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03016      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03017         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03018         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03019         PERFORM 4920-BLD-OP-BC-MH-LIFE-MAX                        GBIFPGM 
03020           VARYING GAA-INDEX                                       GBIFPGM 
03021             FROM 1 BY 1                                           GBIFPGM 
03022               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03606 *                                                                 GBIFPGM 
03607 *4000-4930.                                                       GBIFPGM 
03608 *                                                                 GBIFPGM 
03609 *    IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03610 *       MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03611 *       MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03612 *       PERFORM 4930-BLD-OP-BS-MH-LIFE-MAX                        GBIFPGM 
03613 *         VARYING GAA-INDEX                                       GBIFPGM 
03614 *           FROM 1 BY 1                                           GBIFPGM 
03615 *             UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03616 *    IF WS-CON-ABM-FOUND                                          GBIFPGM 
03617 *       MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03618 *       MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03619 *       PERFORM 4930-BLD-OP-BS-MH-LIFE-MAX                        GBIFPGM 
03620 *         VARYING GAA-INDEX                                       GBIFPGM 
03621 *           FROM 1 BY 1                                           GBIFPGM 
03622 *             UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03040                                                                   GBIFPGM 
03041  4000-4940.                                                       GBIFPGM 
03042                                                                   GBIFPGM 
03043      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03044         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03045         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03046         PERFORM 4940-BLD-OP-BCBS-MH-LIFE-MAX                      GBIFPGM 
03047           VARYING GAA-INDEX                                       GBIFPGM 
03048             FROM 1 BY 1                                           GBIFPGM 
03049               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03050      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03051         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03052         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03053         PERFORM 4940-BLD-OP-BCBS-MH-LIFE-MAX                      GBIFPGM 
03054           VARYING GAA-INDEX                                       GBIFPGM 
03055             FROM 1 BY 1                                           GBIFPGM 
03056               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03057                                                                   GBIFPGM 
03058  4000-4950.                                                       GBIFPGM 
03059                                                                   GBIFPGM 
03060      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
03061         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03062         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03063         PERFORM 4950-BLD-OP-BC-SA-PMT-LVL                         GBIFPGM 
03064           VARYING GAB-INDEX                                       GBIFPGM 
03065             FROM 1 BY 1                                           GBIFPGM 
03066               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03067      IF WS-CON-ACL-FOUND                                          GBIFPGM 
03068         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03069         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03070         PERFORM 4950-BLD-OP-BC-SA-PMT-LVL                         GBIFPGM 
03071           VARYING GAB-INDEX                                       GBIFPGM 
03072             FROM 1 BY 1                                           GBIFPGM 
03073               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03074                                                                   GBIFPGM 
03075 *MQ 10/03                                                         GBIFPGM 
03076  4000-4955.                                                       GBIFPGM 
03077                                                                   GBIFPGM 
03078      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
03079         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03080         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03081         PERFORM 4955-BLD-BC-SA-PMT-LVL-POT                        GBIFPGM 
03082           VARYING GAB-INDEX                                       GBIFPGM 
03083             FROM 1 BY 1                                           GBIFPGM 
03084               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03085      IF WS-CON-ACL-FOUND                                          GBIFPGM 
03086         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03087         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03088         PERFORM 4955-BLD-BC-SA-PMT-LVL-POT                        GBIFPGM 
03089           VARYING GAB-INDEX                                       GBIFPGM 
03090             FROM 1 BY 1                                           GBIFPGM 
03091               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03092                                                                   GBIFPGM 
03093  4000-4960.                                                       GBIFPGM 
03094                                                                   GBIFPGM 
03095      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
03096         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03097         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03098         PERFORM 4960-BLD-OP-BS-SA-PMT-LVL                         GBIFPGM 
03099           VARYING GAB-INDEX                                       GBIFPGM 
03100             FROM 1 BY 1                                           GBIFPGM 
03101               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03102      IF WS-CON-ACL-FOUND                                          GBIFPGM 
03103         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03104         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03105         PERFORM 4960-BLD-OP-BS-SA-PMT-LVL                         GBIFPGM 
03106           VARYING GAB-INDEX                                       GBIFPGM 
03107             FROM 1 BY 1                                           GBIFPGM 
03108               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03109                                                                   GBIFPGM 
03110 *MQ 10/03                                                         GBIFPGM 
03111  4000-4965.                                                       GBIFPGM 
03112                                                                   GBIFPGM 
03113      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
03114         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03115         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03116         PERFORM 4965-BLD-BS-SA-PMT-LVL-POT                        GBIFPGM 
03117           VARYING GAB-INDEX                                       GBIFPGM 
03118             FROM 1 BY 1                                           GBIFPGM 
03119               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03120      IF WS-CON-ACL-FOUND                                          GBIFPGM 
03121         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03122         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03123         PERFORM 4965-BLD-BS-SA-PMT-LVL-POT                        GBIFPGM 
03124           VARYING GAB-INDEX                                       GBIFPGM 
03125             FROM 1 BY 1                                           GBIFPGM 
03126               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03127                                                                   GBIFPGM 
03128  4000-4970.                                                       GBIFPGM 
03129                                                                   GBIFPGM 
03130      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
03131         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03132         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03133         PERFORM 4970-BLD-OP-BCBS-SA-PMT-LVL                       GBIFPGM 
03134           VARYING GAB-INDEX                                       GBIFPGM 
03135             FROM 1 BY 1                                           GBIFPGM 
03136               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03137      IF WS-CON-ACL-FOUND                                          GBIFPGM 
03138         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03139         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03140         PERFORM 4970-BLD-OP-BCBS-SA-PMT-LVL                       GBIFPGM 
03141           VARYING GAB-INDEX                                       GBIFPGM 
03142             FROM 1 BY 1                                           GBIFPGM 
03143               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03144                                                                   GBIFPGM 
03145 *MQ 10/03                                                         GBIFPGM 
03146  4000-4975.                                                       GBIFPGM 
03147                                                                   GBIFPGM 
03148      IF WS-GRP-ACL-FOUND                                          GBIFPGM 
03149         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03150         MOVE WS-GRP-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03151         PERFORM 4975-BLD-BCBS-SA-PMT-LVL-POT                      GBIFPGM 
03152           VARYING GAB-INDEX                                       GBIFPGM 
03153             FROM 1 BY 1                                           GBIFPGM 
03154               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03155      IF WS-CON-ACL-FOUND                                          GBIFPGM 
03156         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03157         MOVE WS-CON-ACL-HOLD TO WS-LOOP-ACL-HOLD                  GBIFPGM 
03158         PERFORM 4975-BLD-BCBS-SA-PMT-LVL-POT                      GBIFPGM 
03159           VARYING GAB-INDEX                                       GBIFPGM 
03160             FROM 1 BY 1                                           GBIFPGM 
03161               UNTIL GAB-INDEX = GAB-ENTRY-COUNT.                  GBIFPGM 
03162                                                                   GBIFPGM 
03163  4000-4980.                                                       GBIFPGM 
03164                                                                   GBIFPGM 
03165      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03166         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03167         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03168         PERFORM 4980-BLD-OP-BC-SA-BEN-PERD-MAX                    GBIFPGM 
03169           VARYING GAA-INDEX                                       GBIFPGM 
03170             FROM 1 BY 1                                           GBIFPGM 
03171               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03172      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03173         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03174         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03175         PERFORM 4980-BLD-OP-BC-SA-BEN-PERD-MAX                    GBIFPGM 
03176           VARYING GAA-INDEX                                       GBIFPGM 
03177             FROM 1 BY 1                                           GBIFPGM 
03178               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03179                                                                   GBIFPGM 
03180  4000-4990.                                                       GBIFPGM 
03181                                                                   GBIFPGM 
03182      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03183         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03184         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03185         PERFORM 4990-BLD-OP-BS-SA-BEN-PERD-MAX                    GBIFPGM 
03186           VARYING GAA-INDEX                                       GBIFPGM 
03187             FROM 1 BY 1                                           GBIFPGM 
03188               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03189      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03190         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03191         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03192         PERFORM 4990-BLD-OP-BS-SA-BEN-PERD-MAX                    GBIFPGM 
03193           VARYING GAA-INDEX                                       GBIFPGM 
03194             FROM 1 BY 1                                           GBIFPGM 
03195               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03196                                                                   GBIFPGM 
03197  4000-5000.                                                       GBIFPGM 
03198                                                                   GBIFPGM 
03199      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03200         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03201         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03202         PERFORM 5000-BLD-OP-BCBS-SA-BP-MAX                        GBIFPGM 
03203           VARYING GAA-INDEX                                       GBIFPGM 
03204             FROM 1 BY 1                                           GBIFPGM 
03205               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03206      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03207         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03208         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03209         PERFORM 5000-BLD-OP-BCBS-SA-BP-MAX                        GBIFPGM 
03210           VARYING GAA-INDEX                                       GBIFPGM 
03211             FROM 1 BY 1                                           GBIFPGM 
03212               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03213                                                                   GBIFPGM 
03214 *MQ 10/03                                                         GBIFPGM 
03215  4000-5002.                                                       GBIFPGM 
03216                                                                   GBIFPGM 
03217      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03218         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03219         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03220         PERFORM 5002-BLD-BCBS-OP-SA-BP-MAX-CH                     GBIFPGM 
03221           VARYING GAA-INDEX                                       GBIFPGM 
03222             FROM 1 BY 1                                           GBIFPGM 
03223               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03224      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03225         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03226         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03227         PERFORM 5002-BLD-BCBS-OP-SA-BP-MAX-CH                     GBIFPGM 
03228           VARYING GAA-INDEX                                       GBIFPGM 
03229             FROM 1 BY 1                                           GBIFPGM 
03230               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03231                                                                   GBIFPGM 
03232 *MQ 10/03                                                         GBIFPGM 
03233  4000-5005.                                                       GBIFPGM 
03234                                                                   GBIFPGM 
03235      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03236         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03237         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03238         PERFORM 5005-BLD-BCBS-OP-SA-BP-MAX-AD                     GBIFPGM 
03239           VARYING GAA-INDEX                                       GBIFPGM 
03240             FROM 1 BY 1                                           GBIFPGM 
03241               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03242      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03243         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03244         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03245         PERFORM 5005-BLD-BCBS-OP-SA-BP-MAX-AD                     GBIFPGM 
03246           VARYING GAA-INDEX                                       GBIFPGM 
03247             FROM 1 BY 1                                           GBIFPGM 
03248               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03249                                                                   GBIFPGM 
03250  4000-5010.                                                       GBIFPGM 
03251                                                                   GBIFPGM 
03252      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03253         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03254         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03255         PERFORM 5010-BLD-OP-BC-SA-LIFETIME-MAX                    GBIFPGM 
03256           VARYING GAA-INDEX                                       GBIFPGM 
03257             FROM 1 BY 1                                           GBIFPGM 
03258               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03259      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03260         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03261         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03262         PERFORM 5010-BLD-OP-BC-SA-LIFETIME-MAX                    GBIFPGM 
03263           VARYING GAA-INDEX                                       GBIFPGM 
03264             FROM 1 BY 1                                           GBIFPGM 
03265               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03266                                                                   GBIFPGM 
03267  4000-5020.                                                       GBIFPGM 
03268                                                                   GBIFPGM 
03269      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03270         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03271         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03272         PERFORM 5020-BLD-OP-BS-SA-LIFETIME-MAX                    GBIFPGM 
03273           VARYING GAA-INDEX                                       GBIFPGM 
03274             FROM 1 BY 1                                           GBIFPGM 
03275               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03276      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03277         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03278         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03279         PERFORM 5020-BLD-OP-BS-SA-LIFETIME-MAX                    GBIFPGM 
03280           VARYING GAA-INDEX                                       GBIFPGM 
03281             FROM 1 BY 1                                           GBIFPGM 
03282               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03283                                                                   GBIFPGM 
03284  4000-5030.                                                       GBIFPGM 
03285                                                                   GBIFPGM 
03286      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03287         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03288         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03289         PERFORM 5030-BLD-OP-BCBS-SA-LIFE-MAX                      GBIFPGM 
03290           VARYING GAA-INDEX                                       GBIFPGM 
03291             FROM 1 BY 1                                           GBIFPGM 
03292               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03293      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03294         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03295         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03296         PERFORM 5030-BLD-OP-BCBS-SA-LIFE-MAX                      GBIFPGM 
03297           VARYING GAA-INDEX                                       GBIFPGM 
03298             FROM 1 BY 1                                           GBIFPGM 
03299               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03300                                                                   GBIFPGM 
03301 *MQ 10/03                                                         GBIFPGM 
03302  4000-5040.                                                       GBIFPGM 
03303                                                                   GBIFPGM 
03304      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03305         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03306         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03307         PERFORM 5040-BLD-HOSP-CARE-BP-MAX                         GBIFPGM 
03308           VARYING GAA-INDEX                                       GBIFPGM 
03309             FROM 1 BY 1                                           GBIFPGM 
03310               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03311      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03312         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03313         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03314         PERFORM 5040-BLD-HOSP-CARE-BP-MAX                         GBIFPGM 
03315           VARYING GAA-INDEX                                       GBIFPGM 
03316             FROM 1 BY 1                                           GBIFPGM 
03317               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03318                                                                   GBIFPGM 
03319 *MQ 10/03                                                         GBIFPGM 
03320  4000-5050.                                                       GBIFPGM 
03321                                                                   GBIFPGM 
03322      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03323         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03324         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03325         PERFORM 5050-BLD-COOR-HM-CARE-BP-MAX                      GBIFPGM 
03326           VARYING GAA-INDEX                                       GBIFPGM 
03327             FROM 1 BY 1                                           GBIFPGM 
03328               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03329      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03330         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03331         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03332         PERFORM 5050-BLD-COOR-HM-CARE-BP-MAX                      GBIFPGM 
03333           VARYING GAA-INDEX                                       GBIFPGM 
03334             FROM 1 BY 1                                           GBIFPGM 
03335               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03336                                                                   GBIFPGM 
03920 *MQ 5/11/05                                                       GBIFPGM 
03921  4000-5052.                                                       GBIFPGM 
03922                                                                   GBIFPGM 
03923      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03924         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03925         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03926         PERFORM 5052-BLD-COOR-HM-CARE-DAY-MAX                     GBIFPGM 
03927           VARYING GAA-INDEX                                       GBIFPGM 
03928             FROM 1 BY 1                                           GBIFPGM 
03929               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03930      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03931         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03932         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03933         PERFORM 5052-BLD-COOR-HM-CARE-DAY-MAX                     GBIFPGM 
03934           VARYING GAA-INDEX                                       GBIFPGM 
03935             FROM 1 BY 1                                           GBIFPGM 
03936               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03937                                                                   GBIFPGM 
03938  4000-5060.                                                       GBIFPGM 
03939                                                                   GBIFPGM 
03940      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03941         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03942         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03943         PERFORM 5060-BLD-SMI-BP-MAX-IP                            GBIFPGM 
03944           VARYING GAA-INDEX                                       GBIFPGM 
03945             FROM 1 BY 1                                           GBIFPGM 
03946               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03947      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03948         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03949         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03950         PERFORM 5060-BLD-SMI-BP-MAX-IP                            GBIFPGM 
03951           VARYING GAA-INDEX                                       GBIFPGM 
03952             FROM 1 BY 1                                           GBIFPGM 
03953               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03954                                                                   GBIFPGM 
03955  4000-5070.                                                       GBIFPGM 
03956                                                                   GBIFPGM 
03957      IF WS-GRP-ABM-FOUND                                          GBIFPGM 
03958         MOVE 'G' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03959         MOVE WS-GRP-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03960         PERFORM 5070-BLD-SMI-BP-MAX-OP                            GBIFPGM 
03961           VARYING GAA-INDEX                                       GBIFPGM 
03962             FROM 1 BY 1                                           GBIFPGM 
03963               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03964      IF WS-CON-ABM-FOUND                                          GBIFPGM 
03965         MOVE 'C' TO WS-PROCESS-CON-GRP-SW                         GBIFPGM 
03966         MOVE WS-CON-ABM-HOLD TO GAA-RECORD                        GBIFPGM 
03967         PERFORM 5070-BLD-SMI-BP-MAX-OP                            GBIFPGM 
03968           VARYING GAA-INDEX                                       GBIFPGM 
03969             FROM 1 BY 1                                           GBIFPGM 
03970               UNTIL GAA-INDEX = GAA-ENTRY-COUNT.                  GBIFPGM 
03971                                                                   GBIFPGM 
03337  4000-900-EXIT.                                                   GBIFPGM 
03338      EXIT.                                                        GBIFPGM 
03339 /                                                                 GBIFPGM 
03340 ******************************************************************GBIFPGM 
03341 *                                                                 GBIFPGM 
03342 *    DEDUCTIBLE PER INDIVIDUAL                                    GBIFPGM 
03343 *                                                                 GBIFPGM 
03344 ******************************************************************GBIFPGM 
03345  4020-BLD-DED-PER-IND.                                            GBIFPGM 
03346                                                                   GBIFPGM 
03347      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'IIDD'                     GBIFPGM 
03348         IF WS-PROCESS-CON                                         GBIFPGM 
03349            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
03350               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03351                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03352               MOVE WS-VALUE-LIMIT-S      TO GCBH-DED-PER-IND-IN   GBIFPGM 
03353 *JP 3/20/03                                                       GBIFPGM 
03354               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03355                                          TO  WS-WRK-VAL-1         GBIFPGM 
03356               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 - 75.00         GBIFPGM 
03357               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
03358               MOVE WS-WRK-VAL-BUX TO GCBH-DED-PER-IND-IN-UTL      GBIFPGM 
03359            ELSE                                                   GBIFPGM 
03360               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03361                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03362               MOVE WS-VALUE-LIMIT        TO GCBH-DED-PER-IND-IN   GBIFPGM 
03363            END-IF                                                 GBIFPGM 
03364         END-IF                                                    GBIFPGM 
03365      END-IF                                                       GBIFPGM 
03366      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'IODD'                     GBIFPGM 
03367         IF WS-PROCESS-GRP                                         GBIFPGM 
03368            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
03369               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03370                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03371               MOVE WS-VALUE-LIMIT-S      TO GCBH-DED-PER-IND-OUT  GBIFPGM 
03372 *JP 3/20/03                                                       GBIFPGM 
03373               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03374                                          TO  WS-WRK-VAL-1         GBIFPGM 
03375               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 - 100.00        GBIFPGM 
03376               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
03377               MOVE WS-WRK-VAL-BUX TO GCBH-DED-PER-IND-OUT-UTL     GBIFPGM 
03378            ELSE                                                   GBIFPGM 
03379               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03380                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03381               MOVE WS-VALUE-LIMIT        TO GCBH-DED-PER-IND-OUT  GBIFPGM 
03382            END-IF                                                 GBIFPGM 
03383         END-IF                                                    GBIFPGM 
03384      END-IF                                                       GBIFPGM 
03385      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'IADD'                     GBIFPGM 
03386         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
03387            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
03388                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
03389            MOVE WS-VALUE-LIMIT-S      TO GCBH-DED-PER-IND-OTH     GBIFPGM 
03390 *JP 3/20/03                                                       GBIFPGM 
03391               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03392                                          TO  WS-WRK-VAL-1         GBIFPGM 
03393               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 - 25.00         GBIFPGM 
03394               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
03395               MOVE WS-WRK-VAL-BUX TO GCBH-DED-PER-IND-OTH-UTL     GBIFPGM 
03396 *JP 3/30/04                                                       GBIFPGM 
03397            IF WS-BLUALT-GRP                                       GBIFPGM 
03398               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03399                 TO WS-SAVE-IADD                                   GBIFPGM 
03400            END-IF                                                 GBIFPGM 
03401         ELSE                                                      GBIFPGM 
03402            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
03403                                       TO WS-VALUE-LIMIT           GBIFPGM 
03404            MOVE WS-VALUE-LIMIT        TO GCBH-DED-PER-IND-OTH     GBIFPGM 
03405         END-IF                                                    GBIFPGM 
03406      END-IF.                                                      GBIFPGM 
03407                                                                   GBIFPGM 
03408  4020-EXIT.                                                       GBIFPGM 
03409      EXIT.                                                        GBIFPGM 
03410 /                                                                 GBIFPGM 
03411 ******************************************************************GBIFPGM 
03412 *                                                                 GBIFPGM 
04048 *    INDIVIDUAL STAND ALONE DEDUCTIBLE                            GBIFPGM 
04049 *                                                                 GBIFPGM 
04050 ******************************************************************GBIFPGM 
04051  4022-BLD-IND-ALONE-DED.                                          GBIFPGM 
04052                                                                   GBIFPGM 
04053      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'IISD'                     GBIFPGM 
04054         IF WS-PROCESS-CON                                         GBIFPGM 
04055            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04056               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04057                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04058               MOVE WS-VALUE-LIMIT-S      TO GCBH-IND-ALONE-DED-IN GBIFPGM 
04059            ELSE                                                   GBIFPGM 
04060               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04061                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04062               MOVE WS-VALUE-LIMIT        TO GCBH-IND-ALONE-DED-IN GBIFPGM 
04063            END-IF                                                 GBIFPGM 
04064         END-IF                                                    GBIFPGM 
04065      END-IF                                                       GBIFPGM 
04066      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'IOSD'                     GBIFPGM 
04067         IF WS-PROCESS-GRP                                         GBIFPGM 
04068            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04069               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04070                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04071               MOVE WS-VALUE-LIMIT-S      TO GCBH-IND-ALONE-DED-OUTGBIFPGM 
04072            ELSE                                                   GBIFPGM 
04073               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04074                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04075               MOVE WS-VALUE-LIMIT        TO GCBH-IND-ALONE-DED-OUTGBIFPGM 
04076            END-IF                                                 GBIFPGM 
04077         END-IF                                                    GBIFPGM 
04078      END-IF.                                                      GBIFPGM 
04079                                                                   GBIFPGM 
04080  4022-EXIT.                                                       GBIFPGM 
04081      EXIT.                                                        GBIFPGM 
04082 /                                                                 GBIFPGM 
04083 ******************************************************************GBIFPGM 
04084 *                                                                 GBIFPGM 
04085 *    COMBINED INDIVIDUAL DEDUCTIBLE                               GBIFPGM 
04086 *                                                                 GBIFPGM 
04087 ******************************************************************GBIFPGM 
04088  4025-BLD-COMB-IND-DED.                                           GBIFPGM 
04089                                                                   GBIFPGM 
04090      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'IICD'                     GBIFPGM 
04091         IF WS-PROCESS-CON                                         GBIFPGM 
04092            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04093               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04094                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04095               MOVE WS-VALUE-LIMIT-S      TO GCBH-COMB-IND-DED-IN  GBIFPGM 
04096            ELSE                                                   GBIFPGM 
04097               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04098                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04099               MOVE WS-VALUE-LIMIT        TO GCBH-COMB-IND-DED-IN  GBIFPGM 
04100            END-IF                                                 GBIFPGM 
04101         END-IF                                                    GBIFPGM 
04102      END-IF                                                       GBIFPGM 
04103      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'IOCD'                     GBIFPGM 
04104         IF WS-PROCESS-GRP                                         GBIFPGM 
04105            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04106               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04107                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04108               MOVE WS-VALUE-LIMIT-S      TO GCBH-COMB-IND-DED-OUT GBIFPGM 
04109            ELSE                                                   GBIFPGM 
04110               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04111                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04112               MOVE WS-VALUE-LIMIT        TO GCBH-COMB-IND-DED-OUT GBIFPGM 
04113            END-IF                                                 GBIFPGM 
04114         END-IF                                                    GBIFPGM 
04115      END-IF.                                                      GBIFPGM 
04116                                                                   GBIFPGM 
04117  4025-EXIT.                                                       GBIFPGM 
04118      EXIT.                                                        GBIFPGM 
04119 /                                                                 GBIFPGM 
04120 ******************************************************************GBIFPGM 
04121 *                                                                 GBIFPGM 
03413 *    DEDUCTIBLE PER FAMILY                                        GBIFPGM 
03414 *                                                                 GBIFPGM 
03415 ******************************************************************GBIFPGM 
03416  4030-BLD-DED-PER-FAM.                                            GBIFPGM 
03417                                                                   GBIFPGM 
03418      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'FIDD'                     GBIFPGM 
03419         IF WS-PROCESS-CON                                         GBIFPGM 
03420            EVALUATE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)          GBIFPGM 
03421              WHEN '5'                                             GBIFPGM 
03422                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
03423                                        TO  WS-VALUE-LIMIT-S       GBIFPGM 
03424                 MOVE WS-VALUE-LIMIT-S  TO GCBH-DED-PER-FAM-IN     GBIFPGM 
03425              WHEN '7'                                             GBIFPGM 
03426                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
03427                                        TO  WS-VALUE-LIMIT-DEC     GBIFPGM 
03428                 MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL          GBIFPGM 
03429                 MOVE WS-PEOPLE         TO GCBH-DED-PER-FAM-IN     GBIFPGM 
03430              WHEN OTHER                                           GBIFPGM 
03431                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
03432                                        TO  WS-VALUE-LIMIT         GBIFPGM 
03433                 MOVE WS-VALUE-LIMIT    TO GCBH-DED-PER-FAM-IN     GBIFPGM 
03434            END-EVALUATE                                           GBIFPGM 
03435         END-IF                                                    GBIFPGM 
03436      END-IF                                                       GBIFPGM 
03437                                                                   GBIFPGM 
03438      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'FODD'                     GBIFPGM 
03439         IF WS-PROCESS-GRP                                         GBIFPGM 
03440            EVALUATE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)          GBIFPGM 
03441               WHEN '5'                                            GBIFPGM 
03442                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
03443                                        TO  WS-VALUE-LIMIT-S       GBIFPGM 
03444                 MOVE WS-VALUE-LIMIT-S  TO GCBH-DED-PER-FAM-OUT    GBIFPGM 
03445               WHEN '7'                                            GBIFPGM 
03446                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
03447                                        TO  WS-VALUE-LIMIT-DEC     GBIFPGM 
03448                 MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL          GBIFPGM 
03449                 MOVE WS-PEOPLE         TO GCBH-DED-PER-FAM-OUT    GBIFPGM 
03450               WHEN OTHER                                          GBIFPGM 
03451                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
03452                                        TO  WS-VALUE-LIMIT         GBIFPGM 
03453                 MOVE WS-VALUE-LIMIT    TO GCBH-DED-PER-FAM-OUT    GBIFPGM 
03454            END-EVALUATE                                           GBIFPGM 
03455         END-IF                                                    GBIFPGM 
03456      END-IF                                                       GBIFPGM 
03457                                                                   GBIFPGM 
03458      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'FADD'                     GBIFPGM 
03459         EVALUATE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)             GBIFPGM 
03460            WHEN '5'                                               GBIFPGM 
03461              MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                GBIFPGM 
03462                                     TO  WS-VALUE-LIMIT-S          GBIFPGM 
03463              MOVE WS-VALUE-LIMIT-S  TO GCBH-DED-PER-FAM-OTH       GBIFPGM 
03464 *JP 3/30/04                                                       GBIFPGM 
03465              IF WS-BLUALT-GRP                                     GBIFPGM 
03466                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
03467                   TO WS-SAVE-FADD                                 GBIFPGM 
03468              END-IF                                               GBIFPGM 
03469            WHEN '7'                                               GBIFPGM 
03470              MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                GBIFPGM 
03471                                     TO  WS-VALUE-LIMIT-DEC        GBIFPGM 
03472              MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL             GBIFPGM 
03473              MOVE WS-PEOPLE         TO GCBH-DED-PER-FAM-OTH       GBIFPGM 
03474            WHEN OTHER                                             GBIFPGM 
03475              MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                GBIFPGM 
03476                                     TO  WS-VALUE-LIMIT            GBIFPGM 
03477              MOVE WS-VALUE-LIMIT    TO GCBH-DED-PER-FAM-OTH       GBIFPGM 
03478         END-EVALUATE                                              GBIFPGM 
03479      END-IF.                                                      GBIFPGM 
03480                                                                   GBIFPGM 
03481                                                                   GBIFPGM 
03482  4030-EXIT.                                                       GBIFPGM 
03483      EXIT.                                                        GBIFPGM 
03484 /                                                                 GBIFPGM 
03485 ******************************************************************GBIFPGM 
03486 *                                                                 GBIFPGM 
04196 *    FAMILY STAND ALONE DEDUCTIBLE                                GBIFPGM 
04197 *                                                                 GBIFPGM 
04198 ******************************************************************GBIFPGM 
04199  4032-BLD-FAM-ALONE-DED.                                          GBIFPGM 
04200                                                                   GBIFPGM 
04201      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'FISD'                     GBIFPGM 
04202         IF WS-PROCESS-CON                                         GBIFPGM 
04203            EVALUATE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)          GBIFPGM 
04204              WHEN '5'                                             GBIFPGM 
04205                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04206                                        TO  WS-VALUE-LIMIT-S       GBIFPGM 
04207                 MOVE WS-VALUE-LIMIT-S  TO GCBH-FAM-ALONE-DED-IN   GBIFPGM 
04208              WHEN '7'                                             GBIFPGM 
04209                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04210                                        TO  WS-VALUE-LIMIT-DEC     GBIFPGM 
04211                 MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL          GBIFPGM 
04212                 MOVE WS-PEOPLE         TO GCBH-FAM-ALONE-DED-IN   GBIFPGM 
04213              WHEN OTHER                                           GBIFPGM 
04214                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04215                                        TO  WS-VALUE-LIMIT         GBIFPGM 
04216                 MOVE WS-VALUE-LIMIT    TO GCBH-FAM-ALONE-DED-IN   GBIFPGM 
04217            END-EVALUATE                                           GBIFPGM 
04218         END-IF                                                    GBIFPGM 
04219      END-IF                                                       GBIFPGM 
04220                                                                   GBIFPGM 
04221      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'FOSD'                     GBIFPGM 
04222         IF WS-PROCESS-GRP                                         GBIFPGM 
04223            EVALUATE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)          GBIFPGM 
04224               WHEN '5'                                            GBIFPGM 
04225                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04226                                        TO  WS-VALUE-LIMIT-S       GBIFPGM 
04227                 MOVE WS-VALUE-LIMIT-S  TO GCBH-FAM-ALONE-DED-OUT  GBIFPGM 
04228               WHEN '7'                                            GBIFPGM 
04229                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04230                                        TO  WS-VALUE-LIMIT-DEC     GBIFPGM 
04231                 MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL          GBIFPGM 
04232                 MOVE WS-PEOPLE         TO GCBH-FAM-ALONE-DED-OUT  GBIFPGM 
04233               WHEN OTHER                                          GBIFPGM 
04234                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04235                                        TO  WS-VALUE-LIMIT         GBIFPGM 
04236                 MOVE WS-VALUE-LIMIT    TO GCBH-FAM-ALONE-DED-OUT  GBIFPGM 
04237            END-EVALUATE                                           GBIFPGM 
04238         END-IF                                                    GBIFPGM 
04239      END-IF.                                                      GBIFPGM 
04240                                                                   GBIFPGM 
04241  4032-EXIT.                                                       GBIFPGM 
04242      EXIT.                                                        GBIFPGM 
04243 /                                                                 GBIFPGM 
04244 ******************************************************************GBIFPGM 
04245 *                                                                 GBIFPGM 
04246 *    COMBINED FAMILY DEDUCTIBLE                                   GBIFPGM 
04247 *                                                                 GBIFPGM 
04248 ******************************************************************GBIFPGM 
04249  4035-BLD-COMB-FAM-DED.                                           GBIFPGM 
04250                                                                   GBIFPGM 
04251      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'FICD'                     GBIFPGM 
04252         IF WS-PROCESS-CON                                         GBIFPGM 
04253            EVALUATE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)          GBIFPGM 
04254              WHEN '5'                                             GBIFPGM 
04255                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04256                                        TO  WS-VALUE-LIMIT-S       GBIFPGM 
04257                 MOVE WS-VALUE-LIMIT-S  TO GCBH-COMB-FAM-DED-IN    GBIFPGM 
04258              WHEN '7'                                             GBIFPGM 
04259                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04260                                        TO  WS-VALUE-LIMIT-DEC     GBIFPGM 
04261                 MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL          GBIFPGM 
04262                 MOVE WS-PEOPLE         TO GCBH-COMB-FAM-DED-IN    GBIFPGM 
04263              WHEN OTHER                                           GBIFPGM 
04264                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04265                                        TO  WS-VALUE-LIMIT         GBIFPGM 
04266                 MOVE WS-VALUE-LIMIT    TO GCBH-COMB-FAM-DED-IN    GBIFPGM 
04267            END-EVALUATE                                           GBIFPGM 
04268         END-IF                                                    GBIFPGM 
04269      END-IF                                                       GBIFPGM 
04270                                                                   GBIFPGM 
04271      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'FOCD'                     GBIFPGM 
04272         IF WS-PROCESS-GRP                                         GBIFPGM 
04273            EVALUATE GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)          GBIFPGM 
04274               WHEN '5'                                            GBIFPGM 
04275                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04276                                        TO  WS-VALUE-LIMIT-S       GBIFPGM 
04277                 MOVE WS-VALUE-LIMIT-S  TO GCBH-COMB-FAM-DED-OUT   GBIFPGM 
04278               WHEN '7'                                            GBIFPGM 
04279                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04280                                        TO  WS-VALUE-LIMIT-DEC     GBIFPGM 
04281                 MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL          GBIFPGM 
04282                 MOVE WS-PEOPLE         TO GCBH-COMB-FAM-DED-OUT   GBIFPGM 
04283               WHEN OTHER                                          GBIFPGM 
04284                 MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)             GBIFPGM 
04285                                        TO  WS-VALUE-LIMIT         GBIFPGM 
04286                 MOVE WS-VALUE-LIMIT    TO GCBH-COMB-FAM-DED-OUT   GBIFPGM 
04287            END-EVALUATE                                           GBIFPGM 
04288         END-IF                                                    GBIFPGM 
04289      END-IF.                                                      GBIFPGM 
04290                                                                   GBIFPGM 
04291  4035-EXIT.                                                       GBIFPGM 
04292      EXIT.                                                        GBIFPGM 
04293 /                                                                 GBIFPGM 
04294 ******************************************************************GBIFPGM 
04295 *                                                                 GBIFPGM 
03487 *    OUT OF POCKET PER INDIVIDUAL                                 GBIFPGM 
03488 *                                                                 GBIFPGM 
03489 ******************************************************************GBIFPGM 
03490  4040-BLD-OOP-PER-IND.                                            GBIFPGM 
03491                                                                   GBIFPGM 
03492      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'IIOP'                    GBIFPGM 
03493         IF WS-PROCESS-CON                                         GBIFPGM 
03494            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
03495               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03496                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03497               MOVE WS-VALUE-LIMIT-S      TO GCBH-OOP-PER-IND-IN   GBIFPGM 
03498 *JP 3/20/03                                                       GBIFPGM 
03499               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03500                                          TO  WS-WRK-VAL-1         GBIFPGM 
03501               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 / 4             GBIFPGM 
03502               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
03503               MOVE WS-WRK-VAL-BUX TO GCBH-OOP-PER-IND-IN-UTL      GBIFPGM 
03504            ELSE                                                   GBIFPGM 
03505               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03506                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03507               MOVE WS-VALUE-LIMIT        TO GCBH-OOP-PER-IND-IN   GBIFPGM 
03508            END-IF                                                 GBIFPGM 
03509         END-IF                                                    GBIFPGM 
03510      END-IF                                                       GBIFPGM 
03511      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'IOOP'                    GBIFPGM 
03512         IF WS-PROCESS-GRP                                         GBIFPGM 
03513            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
03514               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03515                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03516               MOVE WS-VALUE-LIMIT-S      TO GCBH-OOP-PER-IND-OUT  GBIFPGM 
03517 *JP 3/20/03                                                       GBIFPGM 
03518               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03519                                          TO  WS-WRK-VAL-1         GBIFPGM 
03520               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 / 5             GBIFPGM 
03521               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
03522               MOVE WS-WRK-VAL-BUX TO GCBH-OOP-PER-IND-OUT-UTL     GBIFPGM 
03523            ELSE                                                   GBIFPGM 
03524               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03525                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03526               MOVE WS-VALUE-LIMIT        TO GCBH-OOP-PER-IND-OUT  GBIFPGM 
03527            END-IF                                                 GBIFPGM 
03528         END-IF                                                    GBIFPGM 
03529      END-IF                                                       GBIFPGM 
03530      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'IAOP'                    GBIFPGM 
03531         IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'            GBIFPGM 
03532 **JP 3/30/04                                                      GBIFPGM 
03533            IF WS-BLUALT-GRP                                       GBIFPGM 
03534               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03535                                  TO  WS-WRK-VAL-1                 GBIFPGM 
03536               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 +               GBIFPGM 
03537                   WS-SAVE-IADD                                    GBIFPGM 
03538               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
03539               MOVE WS-WRK-VAL-BUX TO GCBH-OOP-PER-IND-OTH         GBIFPGM 
03540            ELSE                                                   GBIFPGM 
03541               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03542                               TO  WS-VALUE-LIMIT-S                GBIFPGM 
03543               MOVE WS-VALUE-LIMIT-S TO GCBH-OOP-PER-IND-OTH       GBIFPGM 
03544            END-IF                                                 GBIFPGM 
03545         ELSE                                                      GBIFPGM 
03546            MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)                 GBIFPGM 
03547                                       TO  WS-VALUE-LIMIT          GBIFPGM 
03548            MOVE WS-VALUE-LIMIT        TO GCBH-OOP-PER-IND-OTH     GBIFPGM 
03549         END-IF                                                    GBIFPGM 
03550      END-IF.                                                      GBIFPGM 
03551                                                                   GBIFPGM 
03552  4040-EXIT.                                                       GBIFPGM 
03553      EXIT.                                                        GBIFPGM 
03554 /                                                                 GBIFPGM 
03555 ******************************************************************GBIFPGM 
03556 *                                                                 GBIFPGM 
04366 *    INDIVIDUAL STAND ALONE OUT OF POCKET                         GBIFPGM 
04367 *                                                                 GBIFPGM 
04368 ******************************************************************GBIFPGM 
04369  4042-BLD-IND-ALONE-OOP.                                          GBIFPGM 
04370                                                                   GBIFPGM 
04371      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'IISP'                    GBIFPGM 
04372         IF WS-PROCESS-CON                                         GBIFPGM 
04373            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
04374               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04375                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04376               MOVE WS-VALUE-LIMIT-S      TO GCBH-IND-ALONE-OOP-IN GBIFPGM 
04377            ELSE                                                   GBIFPGM 
04378               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04379                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04380               MOVE WS-VALUE-LIMIT        TO GCBH-IND-ALONE-OOP-IN GBIFPGM 
04381            END-IF                                                 GBIFPGM 
04382         END-IF                                                    GBIFPGM 
04383      END-IF                                                       GBIFPGM 
04384      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'IOSP'                    GBIFPGM 
04385         IF WS-PROCESS-GRP                                         GBIFPGM 
04386            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
04387               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04388                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04389               MOVE WS-VALUE-LIMIT-S      TO GCBH-IND-ALONE-OOP-OUTGBIFPGM 
04390            ELSE                                                   GBIFPGM 
04391               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04392                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04393               MOVE WS-VALUE-LIMIT        TO GCBH-IND-ALONE-OOP-OUTGBIFPGM 
04394            END-IF                                                 GBIFPGM 
04395         END-IF                                                    GBIFPGM 
04396      END-IF.                                                      GBIFPGM 
04397                                                                   GBIFPGM 
04398  4042-EXIT.                                                       GBIFPGM 
04399      EXIT.                                                        GBIFPGM 
04400 /                                                                 GBIFPGM 
04401 ******************************************************************GBIFPGM 
04402 *                                                                 GBIFPGM 
04403 *    COMBINED INDIVIDUAL OUT OF POCKET                            GBIFPGM 
04404 *                                                                 GBIFPGM 
04405 ******************************************************************GBIFPGM 
04406  4045-BLD-COMB-IND-OOP.                                           GBIFPGM 
04407                                                                   GBIFPGM 
04408      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'IICP'                    GBIFPGM 
04409         IF WS-PROCESS-CON                                         GBIFPGM 
04410            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
04411               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04412                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04413               MOVE WS-VALUE-LIMIT-S      TO GCBH-COMB-IND-OOP-IN  GBIFPGM 
04414            ELSE                                                   GBIFPGM 
04415               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04416                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04417               MOVE WS-VALUE-LIMIT        TO GCBH-COMB-IND-OOP-IN  GBIFPGM 
04418            END-IF                                                 GBIFPGM 
04419         END-IF                                                    GBIFPGM 
04420      END-IF                                                       GBIFPGM 
04421      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'IOCP'                    GBIFPGM 
04422         IF WS-PROCESS-GRP                                         GBIFPGM 
04423            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
04424               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04425                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04426               MOVE WS-VALUE-LIMIT-S      TO GCBH-COMB-IND-OOP-OUT GBIFPGM 
04427            ELSE                                                   GBIFPGM 
04428               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04429                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04430               MOVE WS-VALUE-LIMIT        TO GCBH-COMB-IND-OOP-OUT GBIFPGM 
04431            END-IF                                                 GBIFPGM 
04432         END-IF                                                    GBIFPGM 
04433      END-IF.                                                      GBIFPGM 
04434                                                                   GBIFPGM 
04435  4045-EXIT.                                                       GBIFPGM 
04436      EXIT.                                                        GBIFPGM 
04437 /                                                                 GBIFPGM 
04438 ******************************************************************GBIFPGM 
04439 *                                                                 GBIFPGM 
03557 *    OUT OF POCKET PER FAMILY                                     GBIFPGM 
03558 *                                                                 GBIFPGM 
03559 ******************************************************************GBIFPGM 
03560  4050-BLD-OOP-PER-FAM.                                            GBIFPGM 
03561                                                                   GBIFPGM 
03562      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'FIOP'                    GBIFPGM 
03563         IF WS-PROCESS-CON                                         GBIFPGM 
03564            EVALUATE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)         GBIFPGM 
03565              WHEN  '5'                                            GBIFPGM 
03566               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03567                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
03568               MOVE WS-VALUE-LIMIT-S  TO GCBH-OOP-PER-FAM-IN       GBIFPGM 
03569              WHEN  '7'                                            GBIFPGM 
03570               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03571                                      TO  WS-VALUE-LIMIT-DEC       GBIFPGM 
03572               MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL            GBIFPGM 
03573               MOVE WS-PEOPLE         TO GCBH-OOP-PER-FAM-IN       GBIFPGM 
03574              WHEN OTHER                                           GBIFPGM 
03575               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03576                                      TO  WS-VALUE-LIMIT           GBIFPGM 
03577               MOVE WS-VALUE-LIMIT    TO GCBH-OOP-PER-FAM-IN       GBIFPGM 
03578            END-EVALUATE                                           GBIFPGM 
03579         END-IF                                                    GBIFPGM 
03580      END-IF                                                       GBIFPGM 
03581                                                                   GBIFPGM 
03582      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'FOOP'                    GBIFPGM 
03583         IF WS-PROCESS-GRP                                         GBIFPGM 
03584            EVALUATE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)         GBIFPGM 
03585              WHEN  '5'                                            GBIFPGM 
03586               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03587                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
03588               MOVE WS-VALUE-LIMIT-S  TO GCBH-OOP-PER-FAM-OUT      GBIFPGM 
03589              WHEN  '7'                                            GBIFPGM 
03590               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03591                                      TO  WS-VALUE-LIMIT-DEC       GBIFPGM 
03592               MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL            GBIFPGM 
03593               MOVE WS-PEOPLE         TO GCBH-OOP-PER-FAM-OUT      GBIFPGM 
03594              WHEN OTHER                                           GBIFPGM 
03595               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03596                                      TO  WS-VALUE-LIMIT           GBIFPGM 
03597               MOVE WS-VALUE-LIMIT    TO GCBH-OOP-PER-FAM-OUT      GBIFPGM 
03598            END-EVALUATE                                           GBIFPGM 
03599         END-IF                                                    GBIFPGM 
03600      END-IF                                                       GBIFPGM 
03601                                                                   GBIFPGM 
03602      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'FAOP'                    GBIFPGM 
03603         EVALUATE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)            GBIFPGM 
03604           WHEN  '5'                                               GBIFPGM 
03605 **JP 3/30/04                                                      GBIFPGM 
03606            IF WS-BLUALT-GRP                                       GBIFPGM 
03607               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03608                                  TO  WS-WRK-VAL-1                 GBIFPGM 
03609               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 +               GBIFPGM 
03610                   WS-SAVE-FADD                                    GBIFPGM 
03611               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
03612               MOVE WS-WRK-VAL-BUX TO GCBH-OOP-PER-FAM-OTH         GBIFPGM 
03613            ELSE                                                   GBIFPGM 
03614               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
03615                                TO  WS-VALUE-LIMIT-S               GBIFPGM 
03616               MOVE WS-VALUE-LIMIT-S TO GCBH-OOP-PER-FAM-OTH       GBIFPGM 
03617            END-IF                                                 GBIFPGM 
03618           WHEN  '7'                                               GBIFPGM 
03619            MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)                 GBIFPGM 
03620                                   TO  WS-VALUE-LIMIT-DEC          GBIFPGM 
03621            MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL               GBIFPGM 
03622            MOVE WS-PEOPLE         TO GCBH-OOP-PER-FAM-OTH         GBIFPGM 
03623           WHEN OTHER                                              GBIFPGM 
03624            MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)                 GBIFPGM 
03625                                   TO  WS-VALUE-LIMIT              GBIFPGM 
03626            MOVE WS-VALUE-LIMIT    TO GCBH-OOP-PER-FAM-OTH         GBIFPGM 
03627         END-EVALUATE                                              GBIFPGM 
03628      END-IF.                                                      GBIFPGM 
03629                                                                   GBIFPGM 
03630                                                                   GBIFPGM 
03631  4050-EXIT.                                                       GBIFPGM 
04515      EXIT.                                                        GBIFPGM 
04516 /                                                                 GBIFPGM 
04517 ******************************************************************GBIFPGM 
04518 *                                                                 GBIFPGM 
04519 *    FAMILY STAND ALONE OUT OF POCKET                             GBIFPGM 
04520 *                                                                 GBIFPGM 
04521 ******************************************************************GBIFPGM 
04522  4052-BLD-FAM-ALONE-OOP.                                          GBIFPGM 
04523                                                                   GBIFPGM 
04524      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'FISP'                    GBIFPGM 
04525         IF WS-PROCESS-CON                                         GBIFPGM 
04526            EVALUATE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)         GBIFPGM 
04527              WHEN  '5'                                            GBIFPGM 
04528               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04529                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
04530               MOVE WS-VALUE-LIMIT-S  TO GCBH-FAM-ALONE-OOP-IN     GBIFPGM 
04531              WHEN  '7'                                            GBIFPGM 
04532               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04533                                      TO  WS-VALUE-LIMIT-DEC       GBIFPGM 
04534               MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL            GBIFPGM 
04535               MOVE WS-PEOPLE         TO GCBH-FAM-ALONE-OOP-IN     GBIFPGM 
04536              WHEN OTHER                                           GBIFPGM 
04537               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04538                                      TO  WS-VALUE-LIMIT           GBIFPGM 
04539               MOVE WS-VALUE-LIMIT    TO GCBH-FAM-ALONE-OOP-IN     GBIFPGM 
04540            END-EVALUATE                                           GBIFPGM 
04541         END-IF                                                    GBIFPGM 
04542      END-IF                                                       GBIFPGM 
04543                                                                   GBIFPGM 
04544      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'FOSP'                    GBIFPGM 
04545         IF WS-PROCESS-GRP                                         GBIFPGM 
04546            EVALUATE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)         GBIFPGM 
04547              WHEN  '5'                                            GBIFPGM 
04548               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04549                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
04550               MOVE WS-VALUE-LIMIT-S  TO GCBH-FAM-ALONE-OOP-OUT    GBIFPGM 
04551              WHEN  '7'                                            GBIFPGM 
04552               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04553                                      TO  WS-VALUE-LIMIT-DEC       GBIFPGM 
04554               MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL            GBIFPGM 
04555               MOVE WS-PEOPLE         TO GCBH-FAM-ALONE-OOP-OUT    GBIFPGM 
04556              WHEN OTHER                                           GBIFPGM 
04557               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04558                                      TO  WS-VALUE-LIMIT           GBIFPGM 
04559               MOVE WS-VALUE-LIMIT    TO GCBH-FAM-ALONE-OOP-OUT    GBIFPGM 
04560            END-EVALUATE                                           GBIFPGM 
04561         END-IF                                                    GBIFPGM 
04562      END-IF.                                                      GBIFPGM 
04563                                                                   GBIFPGM 
04564  4052-EXIT.                                                       GBIFPGM 
04565      EXIT.                                                        GBIFPGM 
04566 /                                                                 GBIFPGM 
04567 ******************************************************************GBIFPGM 
04568 *                                                                 GBIFPGM 
04569 *    COMBINED FAMILY OUT OF POCKET                                GBIFPGM 
04570 *                                                                 GBIFPGM 
04571 ******************************************************************GBIFPGM 
04572  4055-BLD-COMB-FAM-OOP.                                           GBIFPGM 
04573                                                                   GBIFPGM 
04574      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'FICP'                    GBIFPGM 
04575         IF WS-PROCESS-CON                                         GBIFPGM 
04576            EVALUATE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)         GBIFPGM 
04577              WHEN  '5'                                            GBIFPGM 
04578               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04579                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
04580               MOVE WS-VALUE-LIMIT-S  TO GCBH-COMB-FAM-OOP-IN      GBIFPGM 
04581              WHEN  '7'                                            GBIFPGM 
04582               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04583                                      TO  WS-VALUE-LIMIT-DEC       GBIFPGM 
04584               MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL            GBIFPGM 
04585               MOVE WS-PEOPLE         TO GCBH-COMB-FAM-OOP-IN      GBIFPGM 
04586              WHEN OTHER                                           GBIFPGM 
04587               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04588                                      TO  WS-VALUE-LIMIT           GBIFPGM 
04589               MOVE WS-VALUE-LIMIT    TO GCBH-COMB-FAM-OOP-IN      GBIFPGM 
04590            END-EVALUATE                                           GBIFPGM 
04591         END-IF                                                    GBIFPGM 
04592      END-IF                                                       GBIFPGM 
04593                                                                   GBIFPGM 
04594      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'FOCP'                    GBIFPGM 
04595         IF WS-PROCESS-GRP                                         GBIFPGM 
04596            EVALUATE GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)         GBIFPGM 
04597              WHEN  '5'                                            GBIFPGM 
04598               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04599                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
04600               MOVE WS-VALUE-LIMIT-S  TO GCBH-COMB-FAM-OOP-OUT     GBIFPGM 
04601              WHEN  '7'                                            GBIFPGM 
04602               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04603                                      TO  WS-VALUE-LIMIT-DEC       GBIFPGM 
04604               MOVE WS-VALUE-LIM-5-7  TO  WS-PEOPLE-VAL            GBIFPGM 
04605               MOVE WS-PEOPLE         TO GCBH-COMB-FAM-OOP-OUT     GBIFPGM 
04606              WHEN OTHER                                           GBIFPGM 
04607               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
04608                                      TO  WS-VALUE-LIMIT           GBIFPGM 
04609               MOVE WS-VALUE-LIMIT    TO GCBH-COMB-FAM-OOP-OUT     GBIFPGM 
04610            END-EVALUATE                                           GBIFPGM 
04611         END-IF                                                    GBIFPGM 
04612      END-IF.                                                      GBIFPGM 
04613                                                                   GBIFPGM 
04614                                                                   GBIFPGM 
04615  4055-EXIT.                                                       GBIFPGM 
03632      EXIT.                                                        GBIFPGM 
03633 /                                                                 GBIFPGM 
03634 ******************************************************************GBIFPGM 
03635 *                                                                 GBIFPGM 
03636 *    LIFETIME MAXIMUM                                             GBIFPGM 
03637 *                                                                 GBIFPGM 
03638 ******************************************************************GBIFPGM 
03639  4060-BLD-LIFETIME-MAX.                                           GBIFPGM 
03640                                                                   GBIFPGM 
03641      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'INLT'                     GBIFPGM 
03642         IF WS-PROCESS-CON                                         GBIFPGM 
03643            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
03644               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
03645                                       TO WS-VALUE-LIMIT-MIL       GBIFPGM 
03646               MOVE WS-VALUE-LIMIT-MIL TO GCBH-LIFE-MAX-IN         GBIFPGM 
03647            ELSE                                                   GBIFPGM 
03648               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
03649                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03650               MOVE WS-VALUE-LIMIT        TO GCBH-LIFE-MAX-IN      GBIFPGM 
03651            END-IF                                                 GBIFPGM 
03652         END-IF                                                    GBIFPGM 
03653      ELSE                                                         GBIFPGM 
03654      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ONLT'                     GBIFPGM 
03655         IF WS-PROCESS-GRP                                         GBIFPGM 
03656            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
03657               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
03658                                       TO WS-VALUE-LIMIT-MIL       GBIFPGM 
03659               MOVE WS-VALUE-LIMIT-MIL TO GCBH-LIFE-MAX-OUT        GBIFPGM 
03660            ELSE                                                   GBIFPGM 
03661               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
03662                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03663               MOVE WS-VALUE-LIMIT        TO GCBH-LIFE-MAX-OUT     GBIFPGM 
03664            END-IF                                                 GBIFPGM 
03665         END-IF                                                    GBIFPGM 
03666      ELSE                                                         GBIFPGM 
03667      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'OALT'                     GBIFPGM 
03668         IF WS-PROCESS-CON                                         GBIFPGM 
03669            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
03670               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
03671                                       TO WS-VALUE-LIMIT-MIL       GBIFPGM 
03672               MOVE WS-VALUE-LIMIT-MIL TO GCBH-LIFE-MAX-OTH        GBIFPGM 
03673 *JP 3/20/03                                                       GBIFPGM 
03674               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
03675                                          TO  WS-WRK-VAL-1         GBIFPGM 
03676               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 - 12800.00      GBIFPGM 
03677               MOVE WS-WRK-VAL-2 TO WS-VALUE-LIMIT-MIL             GBIFPGM 
03678              MOVE WS-VALUE-LIMIT-MIL TO GCBH-LIFE-MAX-OTH-UTL     GBIFPGM 
03679            ELSE                                                   GBIFPGM 
03680               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
03681                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03682               MOVE WS-VALUE-LIMIT        TO GCBH-LIFE-MAX-OTH     GBIFPGM 
03683            END-IF.                                                GBIFPGM 
03684                                                                   GBIFPGM 
03685  4060-EXIT.                                                       GBIFPGM 
03686      EXIT.                                                        GBIFPGM 
03687 /                                                                 GBIFPGM 
03688 ******************************************************************GBIFPGM 
03689 *                                                                 GBIFPGM 
03690 *    EMERGENCY ROOM COPAY                                         GBIFPGM 
03691 *                                                                 GBIFPGM 
03692 ******************************************************************GBIFPGM 
03693  4070-BLD-EMER-RM-COPAY.                                          GBIFPGM 
03694                                                                   GBIFPGM 
03695      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'ERCI'                    GBIFPGM 
03696         IF WS-PROCESS-CON                                         GBIFPGM 
03697            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
03698               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03699                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03700               MOVE WS-VALUE-LIMIT-S      TO GCBH-EMER-RM-COPAY-IN GBIFPGM 
03701 *MQ 6/06/03                                                       GBIFPGM 
03702               MOVE 'Y'                   TO WS-ERCI-VALQUAL5-SW   GBIFPGM 
03703            ELSE                                                   GBIFPGM 
03704               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03705                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03706               MOVE WS-VALUE-LIMIT        TO GCBH-EMER-RM-COPAY-IN GBIFPGM 
03707            END-IF                                                 GBIFPGM 
03708         END-IF                                                    GBIFPGM 
03709      ELSE                                                         GBIFPGM 
03710      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'ERCO'                    GBIFPGM 
03711         IF WS-PROCESS-GRP                                         GBIFPGM 
03712            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
03713               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03714                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03715               MOVE WS-VALUE-LIMIT-S      TO GCBH-EMER-RM-COPAY-OUTGBIFPGM 
03716 *MQ 6/06/03                                                       GBIFPGM 
03717               MOVE 'Y'                   TO WS-ERCO-VALQUAL5-SW   GBIFPGM 
03718            ELSE                                                   GBIFPGM 
03719               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03720                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03721               MOVE WS-VALUE-LIMIT        TO GCBH-EMER-RM-COPAY-OUTGBIFPGM 
03722            END-IF                                                 GBIFPGM 
03723         END-IF                                                    GBIFPGM 
03724      ELSE                                                         GBIFPGM 
03725      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'ERCA'                    GBIFPGM 
03726         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
03727            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
03728                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
03729            MOVE WS-VALUE-LIMIT-S      TO GCBH-EMER-RM-COPAY-OTH   GBIFPGM 
03730 *MQ 6/06/03                                                       GBIFPGM 
03731            MOVE 'Y'                   TO WS-ERCA-VALQUAL5-SW      GBIFPGM 
03732         ELSE                                                      GBIFPGM 
03733            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
03734                                       TO  WS-VALUE-LIMIT          GBIFPGM 
03735            MOVE WS-VALUE-LIMIT        TO GCBH-EMER-RM-COPAY-OTH   GBIFPGM 
03736         END-IF.                                                   GBIFPGM 
03737                                                                   GBIFPGM 
03738  4070-EXIT.                                                       GBIFPGM 
03739      EXIT.                                                        GBIFPGM 
03740 /                                                                 GBIFPGM 
03741  4070A-BLD-EMER-RM-COPAY.                                         GBIFPGM 
03742                                                                   GBIFPGM 
03743 * MQ 6/06/03                                                      GBIFPGM 
03744      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'ERCI'                     GBIFPGM 
03745         IF WS-PROCESS-CON                                         GBIFPGM 
03746            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
03747               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03748                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03749               MOVE WS-VALUE-LIMIT-S      TO GCBH-EMER-RM-COPAY-IN GBIFPGM 
03750            ELSE                                                   GBIFPGM 
03751               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03752                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03753               MOVE WS-VALUE-LIMIT        TO GCBH-EMER-RM-COPAY-IN GBIFPGM 
03754            END-IF                                                 GBIFPGM 
03755         END-IF                                                    GBIFPGM 
03756      ELSE                                                         GBIFPGM 
03757      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'ERCO'                     GBIFPGM 
03758         IF WS-PROCESS-GRP                                         GBIFPGM 
03759            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
03760               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03761                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03762               MOVE WS-VALUE-LIMIT-S      TO GCBH-EMER-RM-COPAY-OUTGBIFPGM 
03763            ELSE                                                   GBIFPGM 
03764               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
03765                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03766               MOVE WS-VALUE-LIMIT        TO GCBH-EMER-RM-COPAY-OUTGBIFPGM 
03767            END-IF                                                 GBIFPGM 
03768         END-IF                                                    GBIFPGM 
03769      ELSE                                                         GBIFPGM 
03770      IF GAC-DEDL-ACCUMID (GAC-INDEX)  = 'ERCA'                    GBIFPGM 
03771         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
03772            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
03773                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
03774            MOVE WS-VALUE-LIMIT-S      TO GCBH-EMER-RM-COPAY-OTH   GBIFPGM 
03775         ELSE                                                      GBIFPGM 
03776            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
03777                                       TO  WS-VALUE-LIMIT          GBIFPGM 
03778            MOVE WS-VALUE-LIMIT        TO GCBH-EMER-RM-COPAY-OTH   GBIFPGM 
03779         END-IF.                                                   GBIFPGM 
03780                                                                   GBIFPGM 
03781  4070A-EXIT.                                                      GBIFPGM 
03782      EXIT.                                                        GBIFPGM 
03783 /                                                                 GBIFPGM 
04768 * MQ 5/11/05                                                      GBIFPGM 
04769 ******************************************************************GBIFPGM 
04770 *                                                                 GBIFPGM 
04771 *    AMBULANCE COPAY (TX HMO GROUPS ONLY)                         GBIFPGM 
04772 *                                                                 GBIFPGM 
04773 ******************************************************************GBIFPGM 
04774  4071-BLD-AMB-COPAY.                                              GBIFPGM 
04775                                                                   GBIFPGM 
04776      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'ACOI'                    GBIFPGM 
04777         IF WS-PROCESS-CON                                         GBIFPGM 
04778            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04779               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04780                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04781               MOVE WS-VALUE-LIMIT-S      TO GCBH-AMB-COPAY-IN     GBIFPGM 
04782               MOVE 'Y'                   TO WS-ACOI-VALQUAL5-SW   GBIFPGM 
04783            ELSE                                                   GBIFPGM 
04784               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04785                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04786               MOVE WS-VALUE-LIMIT        TO GCBH-AMB-COPAY-IN     GBIFPGM 
04787            END-IF                                                 GBIFPGM 
04788         END-IF                                                    GBIFPGM 
04789      ELSE                                                         GBIFPGM 
04790      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'ACOO'                    GBIFPGM 
04791         IF WS-PROCESS-GRP                                         GBIFPGM 
04792            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04793               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04794                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04795               MOVE WS-VALUE-LIMIT-S      TO GCBH-AMB-COPAY-OUT    GBIFPGM 
04796               MOVE 'Y'                   TO WS-ACOO-VALQUAL5-SW   GBIFPGM 
04797            ELSE                                                   GBIFPGM 
04798               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04799                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04800               MOVE WS-VALUE-LIMIT        TO GCBH-AMB-COPAY-OUT    GBIFPGM 
04801            END-IF                                                 GBIFPGM 
04802         END-IF                                                    GBIFPGM 
04803      ELSE                                                         GBIFPGM 
04804      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'ACOA'                    GBIFPGM 
04805         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
04806            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
04807                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
04808            MOVE WS-VALUE-LIMIT-S      TO GCBH-AMB-COPAY-OTH       GBIFPGM 
04809            MOVE 'Y'                   TO WS-ACOA-VALQUAL5-SW      GBIFPGM 
04810         ELSE                                                      GBIFPGM 
04811            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
04812                                       TO  WS-VALUE-LIMIT          GBIFPGM 
04813            MOVE WS-VALUE-LIMIT        TO GCBH-AMB-COPAY-OTH       GBIFPGM 
04814         END-IF.                                                   GBIFPGM 
04815                                                                   GBIFPGM 
04816  4071-EXIT.                                                       GBIFPGM 
04817      EXIT.                                                        GBIFPGM 
04818 /                                                                 GBIFPGM 
04819  4071A-BLD-AMB-COPAY.                                             GBIFPGM 
04820                                                                   GBIFPGM 
04821      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'ACOI'                     GBIFPGM 
04822         IF WS-PROCESS-CON                                         GBIFPGM 
04823            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04824               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04825                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04826               MOVE WS-VALUE-LIMIT-S      TO GCBH-AMB-COPAY-IN     GBIFPGM 
04827            ELSE                                                   GBIFPGM 
04828               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04829                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04830               MOVE WS-VALUE-LIMIT        TO GCBH-AMB-COPAY-IN     GBIFPGM 
04831            END-IF                                                 GBIFPGM 
04832         END-IF                                                    GBIFPGM 
04833      ELSE                                                         GBIFPGM 
04834      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'ACOO'                     GBIFPGM 
04835         IF WS-PROCESS-GRP                                         GBIFPGM 
04836            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04837               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04838                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04839               MOVE WS-VALUE-LIMIT-S      TO GCBH-AMB-COPAY-OUT    GBIFPGM 
04840            ELSE                                                   GBIFPGM 
04841               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04842                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04843               MOVE WS-VALUE-LIMIT        TO GCBH-AMB-COPAY-OUT    GBIFPGM 
04844            END-IF                                                 GBIFPGM 
04845         END-IF                                                    GBIFPGM 
04846      ELSE                                                         GBIFPGM 
04847      IF GAC-DEDL-ACCUMID (GAC-INDEX)  = 'ACOA'                    GBIFPGM 
04848         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
04849            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
04850                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
04851            MOVE WS-VALUE-LIMIT-S      TO GCBH-AMB-COPAY-OTH       GBIFPGM 
04852         ELSE                                                      GBIFPGM 
04853            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
04854                                       TO  WS-VALUE-LIMIT          GBIFPGM 
04855            MOVE WS-VALUE-LIMIT        TO GCBH-AMB-COPAY-OTH       GBIFPGM 
04856         END-IF.                                                   GBIFPGM 
04857                                                                   GBIFPGM 
04858  4071A-EXIT.                                                      GBIFPGM 
04859      EXIT.                                                        GBIFPGM 
04860 /                                                                 GBIFPGM 
04861 * MQ 5/11/05                                                      GBIFPGM 
04862 ******************************************************************GBIFPGM 
04863 *                                                                 GBIFPGM 
04864 *    SKILLED NURSING FACILITY COPAY (TX HMO GROUPS ONLY)          GBIFPGM 
04865 *                                                                 GBIFPGM 
04866 ******************************************************************GBIFPGM 
04867  4072-BLD-SKILL-NUR-FAC-COPAY.                                    GBIFPGM 
04868                                                                   GBIFPGM 
04869      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SNFI'                    GBIFPGM 
04870         IF WS-PROCESS-CON                                         GBIFPGM 
04871            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04872               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04873                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04874               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
04875                    GCBH-SKILL-NUR-FAC-COPAY-IN                    GBIFPGM 
04876               MOVE 'Y'                   TO WS-SNFI-VALQUAL5-SW   GBIFPGM 
04877            ELSE                                                   GBIFPGM 
04878               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04879                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04880               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
04881                    GCBH-SKILL-NUR-FAC-COPAY-IN                    GBIFPGM 
04882            END-IF                                                 GBIFPGM 
04883         END-IF                                                    GBIFPGM 
04884      ELSE                                                         GBIFPGM 
04885      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SNFO'                    GBIFPGM 
04886         IF WS-PROCESS-GRP                                         GBIFPGM 
04887            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04888               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04889                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04890               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
04891                    GCBH-SKILL-NUR-FAC-COPAY-OUT                   GBIFPGM 
04892               MOVE 'Y'                   TO WS-SNFO-VALQUAL5-SW   GBIFPGM 
04893            ELSE                                                   GBIFPGM 
04894               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04895                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04896               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
04897                    GCBH-SKILL-NUR-FAC-COPAY-OUT                   GBIFPGM 
04898            END-IF                                                 GBIFPGM 
04899         END-IF                                                    GBIFPGM 
04900      ELSE                                                         GBIFPGM 
04901      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SNFA'                    GBIFPGM 
04902         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
04903            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
04904                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
04905            MOVE WS-VALUE-LIMIT-S      TO                          GBIFPGM 
04906                 GCBH-SKILL-NUR-FAC-COPAY-OTH                      GBIFPGM 
04907            MOVE 'Y'                   TO WS-SNFA-VALQUAL5-SW      GBIFPGM 
04908         ELSE                                                      GBIFPGM 
04909            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
04910                                       TO  WS-VALUE-LIMIT          GBIFPGM 
04911            MOVE WS-VALUE-LIMIT        TO                          GBIFPGM 
04912                 GCBH-SKILL-NUR-FAC-COPAY-OTH                      GBIFPGM 
04913         END-IF.                                                   GBIFPGM 
04914                                                                   GBIFPGM 
04915  4072-EXIT.                                                       GBIFPGM 
04916      EXIT.                                                        GBIFPGM 
04917 /                                                                 GBIFPGM 
04918  4072A-BLD-SKILL-NUR-FAC-COPAY.                                   GBIFPGM 
04919                                                                   GBIFPGM 
04920      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'SNFI'                     GBIFPGM 
04921         IF WS-PROCESS-CON                                         GBIFPGM 
04922            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04923               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04924                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04925               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
04926                    GCBH-SKILL-NUR-FAC-COPAY-IN                    GBIFPGM 
04927            ELSE                                                   GBIFPGM 
04928               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04929                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04930               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
04931                    GCBH-SKILL-NUR-FAC-COPAY-IN                    GBIFPGM 
04932            END-IF                                                 GBIFPGM 
04933         END-IF                                                    GBIFPGM 
04934      ELSE                                                         GBIFPGM 
04935      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'SNFO'                     GBIFPGM 
04936         IF WS-PROCESS-GRP                                         GBIFPGM 
04937            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04938               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04939                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04940               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
04941                    GCBH-SKILL-NUR-FAC-COPAY-OUT                   GBIFPGM 
04942            ELSE                                                   GBIFPGM 
04943               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04944                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04945               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
04946                    GCBH-SKILL-NUR-FAC-COPAY-OUT                   GBIFPGM 
04947            END-IF                                                 GBIFPGM 
04948         END-IF                                                    GBIFPGM 
04949      ELSE                                                         GBIFPGM 
04950      IF GAC-DEDL-ACCUMID (GAC-INDEX)  = 'SNFA'                    GBIFPGM 
04951         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
04952            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
04953                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
04954            MOVE WS-VALUE-LIMIT-S      TO                          GBIFPGM 
04955                 GCBH-SKILL-NUR-FAC-COPAY-OTH                      GBIFPGM 
04956         ELSE                                                      GBIFPGM 
04957            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
04958                                       TO  WS-VALUE-LIMIT          GBIFPGM 
04959            MOVE WS-VALUE-LIMIT        TO                          GBIFPGM 
04960                 GCBH-SKILL-NUR-FAC-COPAY-OTH                      GBIFPGM 
04961         END-IF.                                                   GBIFPGM 
04962                                                                   GBIFPGM 
04963  4072A-EXIT.                                                      GBIFPGM 
04964      EXIT.                                                        GBIFPGM 
04965 /                                                                 GBIFPGM 
04966 * MQ 5/11/05                                                      GBIFPGM 
04967 ******************************************************************GBIFPGM 
04968 *                                                                 GBIFPGM 
04969 *    OUTPATIENT PSYCH VISIT COPAY (TX HMO GROUPS ONLY)            GBIFPGM 
04970 *                                                                 GBIFPGM 
04971 ******************************************************************GBIFPGM 
04972  4073-BLD-OP-PSYC-VISIT-COPAY.                                    GBIFPGM 
04973                                                                   GBIFPGM 
04974      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'MHCI'                    GBIFPGM 
04975         IF WS-PROCESS-CON                                         GBIFPGM 
04976            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04977               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04978                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04979               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
04980                    GCBH-OP-PSYC-VISIT-COPAY-IN                    GBIFPGM 
04981               MOVE 'Y'                   TO WS-MHCI-VALQUAL5-SW   GBIFPGM 
04982            ELSE                                                   GBIFPGM 
04983               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04984                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04985               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
04986                    GCBH-OP-PSYC-VISIT-COPAY-IN                    GBIFPGM 
04987            END-IF                                                 GBIFPGM 
04988         END-IF                                                    GBIFPGM 
04989      ELSE                                                         GBIFPGM 
04990      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'MHCO'                    GBIFPGM 
04991         IF WS-PROCESS-GRP                                         GBIFPGM 
04992            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04993               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04994                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04995               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
04996                    GCBH-OP-PSYC-VISIT-COPAY-OUT                   GBIFPGM 
04997               MOVE 'Y'                   TO WS-MHCO-VALQUAL5-SW   GBIFPGM 
04998            ELSE                                                   GBIFPGM 
04999               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05000                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05001               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05002                    GCBH-OP-PSYC-VISIT-COPAY-OUT                   GBIFPGM 
05003            END-IF                                                 GBIFPGM 
05004         END-IF                                                    GBIFPGM 
05005      ELSE                                                         GBIFPGM 
05006      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'MHCA'                    GBIFPGM 
05007         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
05008            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
05009                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05010            MOVE WS-VALUE-LIMIT-S      TO                          GBIFPGM 
05011                 GCBH-OP-PSYC-VISIT-COPAY-OTH                      GBIFPGM 
05012            MOVE 'Y'                   TO WS-MHCA-VALQUAL5-SW      GBIFPGM 
05013         ELSE                                                      GBIFPGM 
05014            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
05015                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05016            MOVE WS-VALUE-LIMIT        TO                          GBIFPGM 
05017                 GCBH-OP-PSYC-VISIT-COPAY-OTH                      GBIFPGM 
05018         END-IF.                                                   GBIFPGM 
05019                                                                   GBIFPGM 
05020  4073-EXIT.                                                       GBIFPGM 
05021      EXIT.                                                        GBIFPGM 
05022 /                                                                 GBIFPGM 
05023  4073A-BLD-OP-PSYC-VISIT-COPAY.                                   GBIFPGM 
05024                                                                   GBIFPGM 
05025      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'MHCI'                     GBIFPGM 
05026         IF WS-PROCESS-CON                                         GBIFPGM 
05027            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
05028               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05029                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05030               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05031                    GCBH-OP-PSYC-VISIT-COPAY-IN                    GBIFPGM 
05032            ELSE                                                   GBIFPGM 
05033               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05034                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05035               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05036                    GCBH-OP-PSYC-VISIT-COPAY-IN                    GBIFPGM 
05037            END-IF                                                 GBIFPGM 
05038         END-IF                                                    GBIFPGM 
05039      ELSE                                                         GBIFPGM 
05040      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'MHCO'                     GBIFPGM 
05041         IF WS-PROCESS-GRP                                         GBIFPGM 
05042            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
05043               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05044                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05045               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05046                    GCBH-OP-PSYC-VISIT-COPAY-OUT                   GBIFPGM 
05047            ELSE                                                   GBIFPGM 
05048               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05049                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05050               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05051                    GCBH-OP-PSYC-VISIT-COPAY-OUT                   GBIFPGM 
05052            END-IF                                                 GBIFPGM 
05053         END-IF                                                    GBIFPGM 
05054      ELSE                                                         GBIFPGM 
05055      IF GAC-DEDL-ACCUMID (GAC-INDEX)  = 'MHCA'                    GBIFPGM 
05056         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
05057            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05058                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05059            MOVE WS-VALUE-LIMIT-S      TO                          GBIFPGM 
05060                 GCBH-OP-PSYC-VISIT-COPAY-OTH                      GBIFPGM 
05061         ELSE                                                      GBIFPGM 
05062            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05063                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05064            MOVE WS-VALUE-LIMIT        TO                          GBIFPGM 
05065                 GCBH-OP-PSYC-VISIT-COPAY-OTH                      GBIFPGM 
05066         END-IF.                                                   GBIFPGM 
05067                                                                   GBIFPGM 
05068  4073A-EXIT.                                                      GBIFPGM 
05069      EXIT.                                                        GBIFPGM 
05070 /                                                                 GBIFPGM 
05071 * MQ 5/11/05                                                      GBIFPGM 
05072 ******************************************************************GBIFPGM 
05073 *                                                                 GBIFPGM 
05074 *    INPATIENT PER ADMIT COPAY (TX HMO GROUPS ONLY)               GBIFPGM 
05075 *                                                                 GBIFPGM 
05076 ******************************************************************GBIFPGM 
05077  4074-BLD-IP-PER-ADM-COPAY.                                       GBIFPGM 
05078                                                                   GBIFPGM 
05079      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'PACI'                    GBIFPGM 
05080         IF WS-PROCESS-CON                                         GBIFPGM 
05081            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
05082               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05083                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05084               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05085                    GCBH-IP-PER-ADM-COPAY-IN                       GBIFPGM 
05086               MOVE 'Y'                   TO WS-PACI-VALQUAL5-SW   GBIFPGM 
05087            ELSE                                                   GBIFPGM 
05088               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05089                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05090               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05091                    GCBH-IP-PER-ADM-COPAY-IN                       GBIFPGM 
05092            END-IF                                                 GBIFPGM 
05093         END-IF                                                    GBIFPGM 
05094      ELSE                                                         GBIFPGM 
05095      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'PACO'                    GBIFPGM 
05096         IF WS-PROCESS-GRP                                         GBIFPGM 
05097            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
05098               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05099                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05100               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05101                    GCBH-IP-PER-ADM-COPAY-OUT                      GBIFPGM 
05102               MOVE 'Y'                   TO WS-PACO-VALQUAL5-SW   GBIFPGM 
05103            ELSE                                                   GBIFPGM 
05104               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05105                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05106               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05107                    GCBH-IP-PER-ADM-COPAY-OUT                      GBIFPGM 
05108            END-IF                                                 GBIFPGM 
05109         END-IF                                                    GBIFPGM 
05110      ELSE                                                         GBIFPGM 
05111      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'PACA'                    GBIFPGM 
05112         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
05113            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
05114                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05115            MOVE WS-VALUE-LIMIT-S      TO                          GBIFPGM 
05116                 GCBH-IP-PER-ADM-COPAY-OTH                         GBIFPGM 
05117            MOVE 'Y'                   TO WS-PACA-VALQUAL5-SW      GBIFPGM 
05118         ELSE                                                      GBIFPGM 
05119            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
05120                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05121            MOVE WS-VALUE-LIMIT        TO                          GBIFPGM 
05122                 GCBH-IP-PER-ADM-COPAY-OTH                         GBIFPGM 
05123         END-IF.                                                   GBIFPGM 
05124                                                                   GBIFPGM 
05125  4074-EXIT.                                                       GBIFPGM 
05126      EXIT.                                                        GBIFPGM 
05127 /                                                                 GBIFPGM 
05128  4074A-BLD-IP-PER-ADM-COPAY.                                      GBIFPGM 
05129                                                                   GBIFPGM 
05130      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'PACI'                     GBIFPGM 
05131         IF WS-PROCESS-CON                                         GBIFPGM 
05132            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
05133               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05134                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05135               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05136                    GCBH-IP-PER-ADM-COPAY-IN                       GBIFPGM 
05137            ELSE                                                   GBIFPGM 
05138               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05139                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05140               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05141                    GCBH-IP-PER-ADM-COPAY-IN                       GBIFPGM 
05142            END-IF                                                 GBIFPGM 
05143         END-IF                                                    GBIFPGM 
05144      ELSE                                                         GBIFPGM 
05145      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'PACO'                     GBIFPGM 
05146         IF WS-PROCESS-GRP                                         GBIFPGM 
05147            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
05148               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05149                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05150               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05151                    GCBH-IP-PER-ADM-COPAY-OUT                      GBIFPGM 
05152            ELSE                                                   GBIFPGM 
05153               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05154                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05155               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05156                    GCBH-IP-PER-ADM-COPAY-OUT                      GBIFPGM 
05157            END-IF                                                 GBIFPGM 
05158         END-IF                                                    GBIFPGM 
05159      ELSE                                                         GBIFPGM 
05160      IF GAC-DEDL-ACCUMID (GAC-INDEX)  = 'PACA'                    GBIFPGM 
05161         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
05162            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05163                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05164            MOVE WS-VALUE-LIMIT-S      TO                          GBIFPGM 
05165                 GCBH-IP-PER-ADM-COPAY-OTH                         GBIFPGM 
05166         ELSE                                                      GBIFPGM 
05167            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05168                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05169            MOVE WS-VALUE-LIMIT        TO                          GBIFPGM 
05170                 GCBH-IP-PER-ADM-COPAY-OTH                         GBIFPGM 
05171         END-IF.                                                   GBIFPGM 
05172                                                                   GBIFPGM 
05173  4074A-EXIT.                                                      GBIFPGM 
05174      EXIT.                                                        GBIFPGM 
05175 /                                                                 GBIFPGM 
05176 * MQ 5/11/05                                                      GBIFPGM 
05177 ******************************************************************GBIFPGM 
05178 *                                                                 GBIFPGM 
05179 *    HOME HEALTH VISIT COPAY (TX HMO GROUPS ONLY)                 GBIFPGM 
05180 *                                                                 GBIFPGM 
05181 ******************************************************************GBIFPGM 
05182  4075-BLD-HOME-HLTH-VST-COPAY.                                    GBIFPGM 
05183                                                                   GBIFPGM 
05184      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'HCPI'                    GBIFPGM 
05185         IF WS-PROCESS-CON                                         GBIFPGM 
05186            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
05187               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05188                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05189               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05190                    GCBH-HOME-HLTH-VST-COPAY-IN                    GBIFPGM 
05191               MOVE 'Y'                   TO WS-HCPI-VALQUAL5-SW   GBIFPGM 
05192            ELSE                                                   GBIFPGM 
05193               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05194                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05195               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05196                    GCBH-HOME-HLTH-VST-COPAY-IN                    GBIFPGM 
05197            END-IF                                                 GBIFPGM 
05198         END-IF                                                    GBIFPGM 
05199      ELSE                                                         GBIFPGM 
05200      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'HCPO'                    GBIFPGM 
05201         IF WS-PROCESS-GRP                                         GBIFPGM 
05202            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
05203               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05204                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05205               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05206                    GCBH-HOME-HLTH-VST-COPAY-OUT                   GBIFPGM 
05207               MOVE 'Y'                   TO WS-HCPO-VALQUAL5-SW   GBIFPGM 
05208            ELSE                                                   GBIFPGM 
05209               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
05210                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05211               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05212                    GCBH-HOME-HLTH-VST-COPAY-OUT                   GBIFPGM 
05213            END-IF                                                 GBIFPGM 
05214         END-IF                                                    GBIFPGM 
05215      ELSE                                                         GBIFPGM 
05216      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'HCPA'                    GBIFPGM 
05217         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
05218            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
05219                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05220            MOVE WS-VALUE-LIMIT-S      TO                          GBIFPGM 
05221                 GCBH-HOME-HLTH-VST-COPAY-OTH                      GBIFPGM 
05222            MOVE 'Y'                   TO WS-HCPA-VALQUAL5-SW      GBIFPGM 
05223         ELSE                                                      GBIFPGM 
05224            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
05225                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05226            MOVE WS-VALUE-LIMIT        TO                          GBIFPGM 
05227                 GCBH-HOME-HLTH-VST-COPAY-OTH                      GBIFPGM 
05228         END-IF.                                                   GBIFPGM 
05229                                                                   GBIFPGM 
05230  4075-EXIT.                                                       GBIFPGM 
05231      EXIT.                                                        GBIFPGM 
05232 /                                                                 GBIFPGM 
05233  4075A-BLD-HOME-HLTH-VST-COPAY.                                   GBIFPGM 
05234                                                                   GBIFPGM 
05235      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'HCPI'                     GBIFPGM 
05236         IF WS-PROCESS-CON                                         GBIFPGM 
05237            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
05238               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05239                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05240               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05241                    GCBH-HOME-HLTH-VST-COPAY-IN                    GBIFPGM 
05242            ELSE                                                   GBIFPGM 
05243               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05244                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05245               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05246                    GCBH-HOME-HLTH-VST-COPAY-IN                    GBIFPGM 
05247            END-IF                                                 GBIFPGM 
05248         END-IF                                                    GBIFPGM 
05249      ELSE                                                         GBIFPGM 
05250      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'HCPO'                     GBIFPGM 
05251         IF WS-PROCESS-GRP                                         GBIFPGM 
05252            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
05253               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05254                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05255               MOVE WS-VALUE-LIMIT-S      TO                       GBIFPGM 
05256                    GCBH-HOME-HLTH-VST-COPAY-OUT                   GBIFPGM 
05257            ELSE                                                   GBIFPGM 
05258               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
05259                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05260               MOVE WS-VALUE-LIMIT        TO                       GBIFPGM 
05261                    GCBH-HOME-HLTH-VST-COPAY-OUT                   GBIFPGM 
05262            END-IF                                                 GBIFPGM 
05263         END-IF                                                    GBIFPGM 
05264      ELSE                                                         GBIFPGM 
05265      IF GAC-DEDL-ACCUMID (GAC-INDEX)  = 'HCPA'                    GBIFPGM 
05266         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
05267            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05268                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05269            MOVE WS-VALUE-LIMIT-S      TO                          GBIFPGM 
05270                 GCBH-HOME-HLTH-VST-COPAY-OTH                      GBIFPGM 
05271         ELSE                                                      GBIFPGM 
05272            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05273                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05274            MOVE WS-VALUE-LIMIT        TO                          GBIFPGM 
05275                 GCBH-HOME-HLTH-VST-COPAY-OTH                      GBIFPGM 
05276         END-IF.                                                   GBIFPGM 
05277                                                                   GBIFPGM 
05278  4075A-EXIT.                                                      GBIFPGM 
05279      EXIT.                                                        GBIFPGM 
05280 /                                                                 GBIFPGM 
05281 ******************************************************************GBIFPGM 
05282 *                                                                 GBIFPGM 
05283 *    ALLERGY TESTING/INJECTIONS/SERUM PAYMENT LEVEL               GBIFPGM 
05284 *            (TX HMO GROUPS ONLY)                                 GBIFPGM 
05285 *                                                                 GBIFPGM 
05286 ******************************************************************GBIFPGM 
05287  4076-BLD-ALLERGY-TST-PMT-LVL.                                    GBIFPGM 
05288                                                                   GBIFPGM 
05289      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ALTI'                    GBIFPGM 
05290         IF WS-PROCESS-CON                                         GBIFPGM 
05291            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05292                                  TO WS-PERCENT-VAL                GBIFPGM 
05293            MOVE WS-PERCENT       TO GCBH-ALLERGY-TST-PMT-LVL-IN   GBIFPGM 
05294         END-IF                                                    GBIFPGM 
05295      ELSE                                                         GBIFPGM 
05296      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ALTO'                    GBIFPGM 
05297         IF WS-PROCESS-GRP                                         GBIFPGM 
05298            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05299                                  TO WS-PERCENT-VAL                GBIFPGM 
05300            MOVE WS-PERCENT       TO GCBH-ALLERGY-TST-PMT-LVL-OUT  GBIFPGM 
05301         END-IF                                                    GBIFPGM 
05302      ELSE                                                         GBIFPGM 
05303      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ALTA'                    GBIFPGM 
05304         IF WS-PROCESS-CON                                         GBIFPGM 
05305            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05306                                  TO WS-PERCENT-VAL                GBIFPGM 
05307            MOVE WS-PERCENT       TO GCBH-ALLERGY-TST-PMT-LVL-OTH. GBIFPGM 
05308                                                                   GBIFPGM 
05309  4076-EXIT.                                                       GBIFPGM 
05310      EXIT.                                                        GBIFPGM 
05311 /                                                                 GBIFPGM 
03784 ******************************************************************GBIFPGM 
03785 *                                                                 GBIFPGM 
03786 *    OFFICE VISIT COPAY                                           GBIFPGM 
03787 *                                                                 GBIFPGM 
03788 ******************************************************************GBIFPGM 
03789  4080-BLD-OFF-VISIT-COPAY.                                        GBIFPGM 
03790                                                                   GBIFPGM 
03791      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'OVCI'                    GBIFPGM 
05320         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
05321            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
05322                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05323            MOVE WS-VALUE-LIMIT-S   TO GCBH-OFF-VISIT-COPAY-IN     GBIFPGM 
03797 *MQ 6/10/03                                                       GBIFPGM 
05325            MOVE 'Y'                   TO WS-OVCI-VALQUAL5-SW      GBIFPGM 
05326         ELSE                                                      GBIFPGM 
05327            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
05328                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05329            MOVE WS-VALUE-LIMIT    TO GCBH-OFF-VISIT-COPAY-IN      GBIFPGM 
03804         END-IF                                                    GBIFPGM 
03805      ELSE                                                         GBIFPGM 
03806      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'OVCO'                    GBIFPGM 
03807         IF WS-PROCESS-GRP                                         GBIFPGM 
03808            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
03809               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03810                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03811               MOVE WS-VALUE-LIMIT-S   TO GCBH-OFF-VISIT-COPAY-OUT GBIFPGM 
03812 *MQ 6/10/03                                                       GBIFPGM 
03813               MOVE 'Y'                   TO WS-OVCO-VALQUAL5-SW   GBIFPGM 
03814            ELSE                                                   GBIFPGM 
03815               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03816                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03817               MOVE WS-VALUE-LIMIT     TO GCBH-OFF-VISIT-COPAY-OUT GBIFPGM 
03818            END-IF                                                 GBIFPGM 
03819         END-IF                                                    GBIFPGM 
03820      ELSE                                                         GBIFPGM 
03821      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'OVCA'                    GBIFPGM 
03822         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
03823            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
03824                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
03825            MOVE WS-VALUE-LIMIT-S      TO GCBH-OFF-VISIT-COPAY-OTH GBIFPGM 
03826 *MQ 6/10/03                                                       GBIFPGM 
03827            MOVE 'Y'                   TO WS-OVCA-VALQUAL5-SW      GBIFPGM 
03828         ELSE                                                      GBIFPGM 
03829            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
03830                                       TO  WS-VALUE-LIMIT          GBIFPGM 
03831            MOVE WS-VALUE-LIMIT        TO GCBH-OFF-VISIT-COPAY-OTH GBIFPGM 
03832         END-IF.                                                   GBIFPGM 
03833                                                                   GBIFPGM 
03834  4080-EXIT.                                                       GBIFPGM 
03835      EXIT.                                                        GBIFPGM 
03836 /                                                                 GBIFPGM 
03837  4080A-BLD-OFF-VISIT-COPAY.                                       GBIFPGM 
03838                                                                   GBIFPGM 
03839 * MQ 6/10/03                                                      GBIFPGM 
03840      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'OVCI'                     GBIFPGM 
05367         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
05368            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05369                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05370            MOVE WS-VALUE-LIMIT-S   TO GCBH-OFF-VISIT-COPAY-IN     GBIFPGM 
05371         ELSE                                                      GBIFPGM 
05372            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05373                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05374            MOVE WS-VALUE-LIMIT    TO GCBH-OFF-VISIT-COPAY-IN      GBIFPGM 
03851         END-IF                                                    GBIFPGM 
03852      ELSE                                                         GBIFPGM 
03853      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'OVCO'                     GBIFPGM 
05378         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
05379            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05380                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05381            MOVE WS-VALUE-LIMIT-S   TO GCBH-OFF-VISIT-COPAY-OUT    GBIFPGM 
05382         ELSE                                                      GBIFPGM 
05383            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
05384                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05385            MOVE WS-VALUE-LIMIT     TO GCBH-OFF-VISIT-COPAY-OUT    GBIFPGM 
03864         END-IF                                                    GBIFPGM 
03865      ELSE                                                         GBIFPGM 
03866      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'OVCA'                     GBIFPGM 
03867         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
03868            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
03869                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
03870            MOVE WS-VALUE-LIMIT-S      TO GCBH-OFF-VISIT-COPAY-OTH GBIFPGM 
03871         ELSE                                                      GBIFPGM 
03872            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
03873                                       TO  WS-VALUE-LIMIT          GBIFPGM 
03874            MOVE WS-VALUE-LIMIT        TO GCBH-OFF-VISIT-COPAY-OTH GBIFPGM 
03875         END-IF.                                                   GBIFPGM 
03876                                                                   GBIFPGM 
03877  4080A-EXIT.                                                      GBIFPGM 
03878      EXIT.                                                        GBIFPGM 
03879 /                                                                 GBIFPGM 
03880 ******************************************************************GBIFPGM 
03881 *                                                                 GBIFPGM 
03882 *    OFFICE VISIT PAYMENT LEVEL                                   GBIFPGM 
03883 *                                                                 GBIFPGM 
03884 ******************************************************************GBIFPGM 
03885  4085-BLD-OFF-VISIT-PMT-LVL.                                      GBIFPGM 
03886                                                                   GBIFPGM 
03887      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OVPI'                    GBIFPGM 
03888         IF WS-PROCESS-CON                                         GBIFPGM 
03889            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
03890                                     TO WS-PERCENT-VAL             GBIFPGM 
03891            MOVE WS-PERCENT          TO GCBH-OFF-VISIT-PMT-LVL-IN  GBIFPGM 
03892         END-IF                                                    GBIFPGM 
03893      ELSE                                                         GBIFPGM 
03894      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OVPO'                    GBIFPGM 
03895         IF WS-PROCESS-GRP                                         GBIFPGM 
03896            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
03897                                     TO WS-PERCENT-VAL             GBIFPGM 
03898            MOVE WS-PERCENT          TO GCBH-OFF-VISIT-PMT-LVL-OUT GBIFPGM 
03899         END-IF                                                    GBIFPGM 
03900      ELSE                                                         GBIFPGM 
03901      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OVPA'                    GBIFPGM 
03902         IF WS-PROCESS-CON                                         GBIFPGM 
03903            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
03904                                     TO WS-PERCENT-VAL             GBIFPGM 
03905            MOVE WS-PERCENT          TO GCBH-OFF-VISIT-PMT-LVL-OTH.GBIFPGM 
03906                                                                   GBIFPGM 
03907  4085-EXIT.                                                       GBIFPGM 
03908      EXIT.                                                        GBIFPGM 
03909 /                                                                 GBIFPGM 
03910 ******************************************************************GBIFPGM 
03911 *                                                                 GBIFPGM 
03912 *    OFFICE SURGERY PAYMENT LEVEL                                 GBIFPGM 
03913 *                                                                 GBIFPGM 
03914 ******************************************************************GBIFPGM 
03915 *MQ 10/09/03                                                      GBIFPGM 
03916                                                                   GBIFPGM 
03917  4086-BLD-OFF-SURG-PMT-LVL.                                       GBIFPGM 
03918                                                                   GBIFPGM 
03919      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OVSI'                    GBIFPGM 
03920         IF WS-PROCESS-CON                                         GBIFPGM 
03921            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
03922                                     TO WS-PERCENT-VAL             GBIFPGM 
03923            MOVE WS-PERCENT          TO GCBH-OFF-SURG-PMT-LVL-IN   GBIFPGM 
03924         END-IF                                                    GBIFPGM 
03925      ELSE                                                         GBIFPGM 
03926      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OVSO'                    GBIFPGM 
03927         IF WS-PROCESS-GRP                                         GBIFPGM 
03928            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
03929                                     TO WS-PERCENT-VAL             GBIFPGM 
03930            MOVE WS-PERCENT          TO GCBH-OFF-SURG-PMT-LVL-OUT  GBIFPGM 
03931         END-IF                                                    GBIFPGM 
03932      ELSE                                                         GBIFPGM 
03933      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OVSA'                    GBIFPGM 
03934         IF WS-PROCESS-CON                                         GBIFPGM 
03935            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
03936                                     TO WS-PERCENT-VAL             GBIFPGM 
03937            MOVE WS-PERCENT          TO GCBH-OFF-SURG-PMT-LVL-OTH  GBIFPGM 
03938          END-IF.                                                  GBIFPGM 
03939                                                                   GBIFPGM 
03940  4086-EXIT.                                                       GBIFPGM 
03941      EXIT.                                                        GBIFPGM 
03942 /                                                                 GBIFPGM 
03943 ******************************************************************GBIFPGM 
03944 *                                                                 GBIFPGM 
03945 *    WELL CARE COPAY                                              GBIFPGM 
03946 *                                                                 GBIFPGM 
03947 ******************************************************************GBIFPGM 
03948  4090-BLD-WELL-CARE-COPAY.                                        GBIFPGM 
03949                                                                   GBIFPGM 
03950      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'WCCI'                    GBIFPGM 
03951         IF WS-PROCESS-CON                                         GBIFPGM 
03952            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
03953               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03954                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03955               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-CARE-COPAY-IN  GBIFPGM 
03956 *MQ 6/10/03                                                       GBIFPGM 
03957               MOVE 'Y'                TO WS-WCCI-VALQUAL5-SW      GBIFPGM 
03958            ELSE                                                   GBIFPGM 
03959               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03960                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03961               MOVE WS-VALUE-LIMIT     TO GCBH-WELL-CARE-COPAY-IN  GBIFPGM 
03962            END-IF                                                 GBIFPGM 
03963         END-IF                                                    GBIFPGM 
03964      ELSE                                                         GBIFPGM 
03965      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'WCCO'                    GBIFPGM 
03966         IF WS-PROCESS-GRP                                         GBIFPGM 
03967            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
03968               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03969                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
03970               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-CARE-COPAY-OUT GBIFPGM 
03971 *MQ 6/10/03                                                       GBIFPGM 
03972               MOVE 'Y'                TO WS-WCCO-VALQUAL5-SW      GBIFPGM 
03973            ELSE                                                   GBIFPGM 
03974               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
03975                                          TO  WS-VALUE-LIMIT       GBIFPGM 
03976               MOVE WS-VALUE-LIMIT     TO GCBH-WELL-CARE-COPAY-OUT GBIFPGM 
03977            END-IF                                                 GBIFPGM 
03978         END-IF                                                    GBIFPGM 
03979      ELSE                                                         GBIFPGM 
03980      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'WCCA'                    GBIFPGM 
03981         IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'            GBIFPGM 
03982            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
03983                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
03984            MOVE WS-VALUE-LIMIT-S      TO GCBH-WELL-CARE-COPAY-OTH GBIFPGM 
03985 *MQ 6/10/03                                                       GBIFPGM 
03986            MOVE 'Y'                   TO WS-WCCA-VALQUAL5-SW      GBIFPGM 
03987         ELSE                                                      GBIFPGM 
03988            MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)                 GBIFPGM 
03989                                       TO  WS-VALUE-LIMIT          GBIFPGM 
03990            MOVE WS-VALUE-LIMIT        TO GCBH-WELL-CARE-COPAY-OTH GBIFPGM 
03991         END-IF.                                                   GBIFPGM 
03992                                                                   GBIFPGM 
03993  4090-EXIT.                                                       GBIFPGM 
03994      EXIT.                                                        GBIFPGM 
03995 /                                                                 GBIFPGM 
03996  4090A-BLD-WELL-CARE-COPAY.                                       GBIFPGM 
03997                                                                   GBIFPGM 
03998      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'WCCI'                     GBIFPGM 
03999         IF WS-PROCESS-CON                                         GBIFPGM 
04000            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04001               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04002                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04003               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-CARE-COPAY-IN  GBIFPGM 
04004            ELSE                                                   GBIFPGM 
04005               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04006                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04007               MOVE WS-VALUE-LIMIT     TO GCBH-WELL-CARE-COPAY-IN  GBIFPGM 
04008            END-IF                                                 GBIFPGM 
04009         END-IF                                                    GBIFPGM 
04010      ELSE                                                         GBIFPGM 
04011      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'WCCO'                     GBIFPGM 
04012         IF WS-PROCESS-GRP                                         GBIFPGM 
04013            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04014               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04015                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04016               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-CARE-COPAY-OUT GBIFPGM 
04017            ELSE                                                   GBIFPGM 
04018               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04019                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04020               MOVE WS-VALUE-LIMIT     TO GCBH-WELL-CARE-COPAY-OUT GBIFPGM 
04021            END-IF                                                 GBIFPGM 
04022         END-IF                                                    GBIFPGM 
04023      ELSE                                                         GBIFPGM 
04024      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'WCCA'                     GBIFPGM 
04025         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'            GBIFPGM 
04026            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
04027                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
04028            MOVE WS-VALUE-LIMIT-S      TO GCBH-WELL-CARE-COPAY-OTH GBIFPGM 
04029         ELSE                                                      GBIFPGM 
04030            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
04031                                       TO  WS-VALUE-LIMIT          GBIFPGM 
04032            MOVE WS-VALUE-LIMIT        TO GCBH-WELL-CARE-COPAY-OTH GBIFPGM 
04033         END-IF.                                                   GBIFPGM 
04034                                                                   GBIFPGM 
04035  4090A-EXIT.                                                      GBIFPGM 
04036      EXIT.                                                        GBIFPGM 
04037 /                                                                 GBIFPGM 
04038 ******************************************************************GBIFPGM 
04039 *                                                                 GBIFPGM 
04040 *    SPECIALIST OFFICE VISIT COPAY                                GBIFPGM 
04041 *                                                                 GBIFPGM 
04042 ******************************************************************GBIFPGM 
04043  4091-BLD-SPEC-OV-COPAY.                                          GBIFPGM 
04044                                                                   GBIFPGM 
04045 *MQ 10/09/03                                                      GBIFPGM 
04046                                                                   GBIFPGM 
04047      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SOVI'                    GBIFPGM 
04048         IF WS-PROCESS-CON                                         GBIFPGM 
04049            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04050               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04051                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04052               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04053                              TO  GCBH-SPEC-OFF-VISIT-COPAY-IN     GBIFPGM 
04054               MOVE 'Y'                TO WS-SOVI-VALQUAL5-SW      GBIFPGM 
04055            ELSE                                                   GBIFPGM 
04056               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04057                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04058               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04059                              TO  GCBH-SPEC-OFF-VISIT-COPAY-IN     GBIFPGM 
04060            END-IF                                                 GBIFPGM 
04061         END-IF                                                    GBIFPGM 
04062      ELSE                                                         GBIFPGM 
04063      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SOVO'                    GBIFPGM 
04064         IF WS-PROCESS-GRP                                         GBIFPGM 
04065            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04066               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04067                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04068               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04069                              TO  GCBH-SPEC-OFF-VISIT-COPAY-OUT    GBIFPGM 
04070               MOVE 'Y'                TO WS-SOVO-VALQUAL5-SW      GBIFPGM 
04071            ELSE                                                   GBIFPGM 
04072               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04073                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04074               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04075                              TO  GCBH-SPEC-OFF-VISIT-COPAY-OUT    GBIFPGM 
04076            END-IF                                                 GBIFPGM 
04077         END-IF                                                    GBIFPGM 
04078      ELSE                                                         GBIFPGM 
04079      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SOVA'                    GBIFPGM 
04080         IF WS-PROCESS-CON                                         GBIFPGM 
04081            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04082               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04083                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04084               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04085                                 TO  GCBH-SPEC-OFF-VISIT-COPAY-OTH GBIFPGM 
04086               MOVE 'Y'                   TO WS-SOVA-VALQUAL5-SW   GBIFPGM 
04087            ELSE                                                   GBIFPGM 
04088               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04089                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04090               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04091                                 TO  GCBH-SPEC-OFF-VISIT-COPAY-OTH GBIFPGM 
04092            END-IF                                                 GBIFPGM 
04093         END-IF                                                    GBIFPGM 
04094      END-IF.                                                      GBIFPGM 
04095                                                                   GBIFPGM 
04096  4091-EXIT.                                                       GBIFPGM 
04097      EXIT.                                                        GBIFPGM 
04098 /                                                                 GBIFPGM 
04099  4091A-BLD-SPEC-OV-COPAY.                                         GBIFPGM 
04100                                                                   GBIFPGM 
04101 *MQ 10/09/03                                                      GBIFPGM 
04102      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'SOVI'                     GBIFPGM 
04103         IF WS-PROCESS-CON                                         GBIFPGM 
04104            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04105               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04106                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04107               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04108                              TO  GCBH-SPEC-OFF-VISIT-COPAY-IN     GBIFPGM 
04109            ELSE                                                   GBIFPGM 
04110               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04111                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04112               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04113                              TO  GCBH-SPEC-OFF-VISIT-COPAY-IN     GBIFPGM 
04114            END-IF                                                 GBIFPGM 
04115         END-IF                                                    GBIFPGM 
04116      ELSE                                                         GBIFPGM 
04117      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'SOVO'                     GBIFPGM 
04118         IF WS-PROCESS-GRP                                         GBIFPGM 
04119            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04120               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04121                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04122               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04123                              TO  GCBH-SPEC-OFF-VISIT-COPAY-OUT    GBIFPGM 
04124            ELSE                                                   GBIFPGM 
04125               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04126                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04127               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04128                              TO  GCBH-SPEC-OFF-VISIT-COPAY-OUT    GBIFPGM 
04129            END-IF                                                 GBIFPGM 
04130         END-IF                                                    GBIFPGM 
04131      ELSE                                                         GBIFPGM 
04132      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'SOVA'                     GBIFPGM 
04133         IF WS-PROCESS-CON                                         GBIFPGM 
04134            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04135               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04136                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04137               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04138                                 TO  GCBH-SPEC-OFF-VISIT-COPAY-OTH GBIFPGM 
04139            ELSE                                                   GBIFPGM 
04140               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04141                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04142               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04143                                 TO  GCBH-SPEC-OFF-VISIT-COPAY-OTH GBIFPGM 
04144            END-IF                                                 GBIFPGM 
04145         END-IF.                                                   GBIFPGM 
04146                                                                   GBIFPGM 
04147  4091A-EXIT.                                                      GBIFPGM 
04148      EXIT.                                                        GBIFPGM 
04149 /                                                                 GBIFPGM 
04150 ******************************************************************GBIFPGM 
04151 *                                                                 GBIFPGM 
04152 *    OUTPATIENT SURGERY COPAY                                     GBIFPGM 
04153 *                                                                 GBIFPGM 
04154 ******************************************************************GBIFPGM 
04155  4092-BLD-OP-SURG-COPAY.                                          GBIFPGM 
04156                                                                   GBIFPGM 
04157 *MQ 10/09/03                                                      GBIFPGM 
04158                                                                   GBIFPGM 
04159      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SCOI'                    GBIFPGM 
04160         IF WS-PROCESS-CON                                         GBIFPGM 
04161            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04162               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04163                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04164               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04165                              TO  GCBH-OP-SURG-COPAY-IN            GBIFPGM 
04166               MOVE 'Y'                TO WS-SCOI-VALQUAL5-SW      GBIFPGM 
04167            ELSE                                                   GBIFPGM 
04168               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04169                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04170               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04171                              TO  GCBH-OP-SURG-COPAY-IN            GBIFPGM 
04172            END-IF                                                 GBIFPGM 
04173         END-IF                                                    GBIFPGM 
04174      ELSE                                                         GBIFPGM 
04175      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SCOO'                    GBIFPGM 
04176         IF WS-PROCESS-GRP                                         GBIFPGM 
04177            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04178               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04179                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04180               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04181                              TO  GCBH-OP-SURG-COPAY-OUT           GBIFPGM 
04182               MOVE 'Y'                TO WS-SCOO-VALQUAL5-SW      GBIFPGM 
04183            ELSE                                                   GBIFPGM 
04184               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04185                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04186               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04187                              TO  GCBH-OP-SURG-COPAY-OUT           GBIFPGM 
04188            END-IF                                                 GBIFPGM 
04189         END-IF                                                    GBIFPGM 
04190      ELSE                                                         GBIFPGM 
04191      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'SCOA'                    GBIFPGM 
04192         IF WS-PROCESS-CON                                         GBIFPGM 
04193            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04194               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04195                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04196               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04197                                 TO  GCBH-OP-SURG-COPAY-OTH        GBIFPGM 
04198               MOVE 'Y'                   TO WS-SCOA-VALQUAL5-SW   GBIFPGM 
04199            ELSE                                                   GBIFPGM 
04200               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04201                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04202               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04203                                 TO  GCBH-OP-SURG-COPAY-OTH        GBIFPGM 
04204            END-IF                                                 GBIFPGM 
04205         END-IF.                                                   GBIFPGM 
04206                                                                   GBIFPGM 
04207  4092-EXIT.                                                       GBIFPGM 
04208      EXIT.                                                        GBIFPGM 
04209 /                                                                 GBIFPGM 
04210  4092A-BLD-OP-SURG-COPAY.                                         GBIFPGM 
04211                                                                   GBIFPGM 
04212 *MQ 10/09/03                                                      GBIFPGM 
04213      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'SCOI'                     GBIFPGM 
04214         IF WS-PROCESS-CON                                         GBIFPGM 
04215            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04216               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04217                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04218               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04219                              TO  GCBH-OP-SURG-COPAY-IN            GBIFPGM 
04220            ELSE                                                   GBIFPGM 
04221               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04222                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04223               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04224                              TO  GCBH-OP-SURG-COPAY-IN            GBIFPGM 
04225            END-IF                                                 GBIFPGM 
04226         END-IF                                                    GBIFPGM 
04227      ELSE                                                         GBIFPGM 
04228      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'SCOO'                     GBIFPGM 
04229         IF WS-PROCESS-GRP                                         GBIFPGM 
04230            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04231               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04232                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04233               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04234                              TO  GCBH-OP-SURG-COPAY-OUT           GBIFPGM 
04235            ELSE                                                   GBIFPGM 
04236               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04237                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04238               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04239                              TO  GCBH-OP-SURG-COPAY-OUT           GBIFPGM 
04240            END-IF                                                 GBIFPGM 
04241         END-IF                                                    GBIFPGM 
04242      ELSE                                                         GBIFPGM 
04243      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'SCOA'                     GBIFPGM 
04244         IF WS-PROCESS-CON                                         GBIFPGM 
04245            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04246               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04247                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04248               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04249                                 TO  GCBH-OP-SURG-COPAY-OTH        GBIFPGM 
04250            ELSE                                                   GBIFPGM 
04251               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04252                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04253               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04254                                 TO  GCBH-OP-SURG-COPAY-OTH        GBIFPGM 
04255            END-IF                                                 GBIFPGM 
04256         END-IF.                                                   GBIFPGM 
04257                                                                   GBIFPGM 
04258  4092A-EXIT.                                                      GBIFPGM 
04259      EXIT.                                                        GBIFPGM 
04260 /                                                                 GBIFPGM 
04261 ******************************************************************GBIFPGM 
04262 *                                                                 GBIFPGM 
04263 *    OUTPATIENT MENTAL HEALTH AND SUBSTANCE ABUSE                 GBIFPGM 
04264 *                                                                 GBIFPGM 
04265 ******************************************************************GBIFPGM 
04266  4093-BLD-OP-MSA-COPAY.                                           GBIFPGM 
04267 *MQ 10/09/03                                                      GBIFPGM 
04268                                                                   GBIFPGM 
04269      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'MSCI'                    GBIFPGM 
04270         IF WS-PROCESS-CON                                         GBIFPGM 
04271            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04272               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04273                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04274               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04275                              TO  GCBH-OP-MSA-COPAY-IN             GBIFPGM 
04276               MOVE 'Y'                TO WS-MSCI-VALQUAL5-SW      GBIFPGM 
04277            ELSE                                                   GBIFPGM 
04278               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04279                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04280               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04281                              TO  GCBH-OP-MSA-COPAY-IN             GBIFPGM 
04282            END-IF                                                 GBIFPGM 
04283         END-IF                                                    GBIFPGM 
04284      ELSE                                                         GBIFPGM 
04285      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'MSCO'                    GBIFPGM 
04286         IF WS-PROCESS-GRP                                         GBIFPGM 
04287            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04288               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04289                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04290               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04291                              TO  GCBH-OP-MSA-COPAY-OUT            GBIFPGM 
04292               MOVE 'Y'                TO WS-MSCO-VALQUAL5-SW      GBIFPGM 
04293            ELSE                                                   GBIFPGM 
04294               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04295                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04296               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04297                              TO  GCBH-OP-MSA-COPAY-OUT            GBIFPGM 
04298            END-IF                                                 GBIFPGM 
04299         END-IF                                                    GBIFPGM 
04300      ELSE                                                         GBIFPGM 
04301      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'MSCA'                    GBIFPGM 
04302         IF WS-PROCESS-CON                                         GBIFPGM 
04303            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04304               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04305                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04306               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04307                                 TO  GCBH-OP-MSA-COPAY-OTH         GBIFPGM 
04308               MOVE 'Y'                   TO WS-MSCA-VALQUAL5-SW   GBIFPGM 
04309            ELSE                                                   GBIFPGM 
04310               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04311                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04312               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04313                                 TO  GCBH-OP-MSA-COPAY-OTH         GBIFPGM 
04314            END-IF                                                 GBIFPGM 
04315         END-IF.                                                   GBIFPGM 
04316                                                                   GBIFPGM 
04317  4093-EXIT.                                                       GBIFPGM 
04318      EXIT.                                                        GBIFPGM 
04319 /                                                                 GBIFPGM 
04320  4093A-BLD-OP-MSA-COPAY.                                          GBIFPGM 
04321                                                                   GBIFPGM 
04322 *MQ 10/09/03                                                      GBIFPGM 
04323      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'MSCI'                     GBIFPGM 
04324         IF WS-PROCESS-CON                                         GBIFPGM 
04325            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04326               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04327                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04328               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04329                              TO  GCBH-OP-MSA-COPAY-IN             GBIFPGM 
04330            ELSE                                                   GBIFPGM 
04331               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04332                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04333               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04334                              TO  GCBH-OP-MSA-COPAY-IN             GBIFPGM 
04335            END-IF                                                 GBIFPGM 
04336         END-IF                                                    GBIFPGM 
04337      ELSE                                                         GBIFPGM 
04338      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'MSCO'                     GBIFPGM 
04339         IF WS-PROCESS-GRP                                         GBIFPGM 
04340            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04341               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04342                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04343               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04344                              TO  GCBH-OP-MSA-COPAY-OUT            GBIFPGM 
04345            ELSE                                                   GBIFPGM 
04346               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04347                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04348               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04349                              TO  GCBH-OP-MSA-COPAY-OUT            GBIFPGM 
04350            END-IF                                                 GBIFPGM 
04351         END-IF                                                    GBIFPGM 
04352      ELSE                                                         GBIFPGM 
04353      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'MSCA'                     GBIFPGM 
04354         IF WS-PROCESS-CON                                         GBIFPGM 
04355            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04356               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04357                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04358               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04359                                 TO  GCBH-OP-MSA-COPAY-OTH         GBIFPGM 
04360            ELSE                                                   GBIFPGM 
04361               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04362                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04363               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04364                                 TO  GCBH-OP-MSA-COPAY-OTH         GBIFPGM 
04365            END-IF                                                 GBIFPGM 
04366         END-IF.                                                   GBIFPGM 
04367                                                                   GBIFPGM 
04368  4093A-EXIT.                                                      GBIFPGM 
04369      EXIT.                                                        GBIFPGM 
04370 /                                                                 GBIFPGM 
04371 ******************************************************************GBIFPGM 
04372 *                                                                 GBIFPGM 
04373 *    URGENT CARE FACILITY COPAY                                   GBIFPGM 
04374 *                                                                 GBIFPGM 
04375 ******************************************************************GBIFPGM 
04376  4094-BLD-UCF-COPAY.                                              GBIFPGM 
04377 *MQ 10/09/03                                                      GBIFPGM 
04378                                                                   GBIFPGM 
04379      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'UCCI'                    GBIFPGM 
04380         IF WS-PROCESS-CON                                         GBIFPGM 
04381            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04382               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04383                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04384               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04385                              TO  GCBH-UCF-COPAY-IN                GBIFPGM 
04386               MOVE 'Y'                TO WS-UCCI-VALQUAL5-SW      GBIFPGM 
04387            ELSE                                                   GBIFPGM 
04388               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04389                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04390               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04391                              TO  GCBH-UCF-COPAY-IN                GBIFPGM 
04392            END-IF                                                 GBIFPGM 
04393         END-IF                                                    GBIFPGM 
04394      ELSE                                                         GBIFPGM 
04395      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'UCCO'                    GBIFPGM 
04396         IF WS-PROCESS-GRP                                         GBIFPGM 
04397            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04398               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04399                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04400               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04401                              TO  GCBH-UCF-COPAY-OUT               GBIFPGM 
04402               MOVE 'Y'                TO WS-UCCO-VALQUAL5-SW      GBIFPGM 
04403            ELSE                                                   GBIFPGM 
04404               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04405                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04406               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04407                              TO  GCBH-UCF-COPAY-OUT               GBIFPGM 
04408            END-IF                                                 GBIFPGM 
04409         END-IF                                                    GBIFPGM 
04410      ELSE                                                         GBIFPGM 
04411      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'UCCA'                    GBIFPGM 
04412         IF WS-PROCESS-CON                                         GBIFPGM 
04413            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04414               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04415                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04416               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04417                                 TO  GCBH-UCF-COPAY-OTH            GBIFPGM 
04418               MOVE 'Y'                   TO WS-UCCA-VALQUAL5-SW   GBIFPGM 
04419            ELSE                                                   GBIFPGM 
04420               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04421                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04422               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04423                                 TO  GCBH-UCF-COPAY-OTH            GBIFPGM 
04424            END-IF                                                 GBIFPGM 
04425         END-IF.                                                   GBIFPGM 
04426                                                                   GBIFPGM 
04427  4094-EXIT.                                                       GBIFPGM 
04428      EXIT.                                                        GBIFPGM 
04429 /                                                                 GBIFPGM 
04430  4094A-BLD-UCF-COPAY.                                             GBIFPGM 
04431                                                                   GBIFPGM 
04432 *MQ 10/09/03                                                      GBIFPGM 
04433      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'UCCI'                     GBIFPGM 
04434         IF WS-PROCESS-CON                                         GBIFPGM 
04435            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04436               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04437                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04438               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04439                              TO  GCBH-UCF-COPAY-IN                GBIFPGM 
04440            ELSE                                                   GBIFPGM 
04441               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04442                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04443               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04444                              TO  GCBH-UCF-COPAY-IN                GBIFPGM 
04445            END-IF                                                 GBIFPGM 
04446         END-IF                                                    GBIFPGM 
04447      ELSE                                                         GBIFPGM 
04448      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'UCCO'                     GBIFPGM 
04449         IF WS-PROCESS-GRP                                         GBIFPGM 
04450            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04451               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04452                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04453               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04454                              TO  GCBH-UCF-COPAY-OUT               GBIFPGM 
04455            ELSE                                                   GBIFPGM 
04456               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04457                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04458               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04459                              TO  GCBH-UCF-COPAY-OUT               GBIFPGM 
04460            END-IF                                                 GBIFPGM 
04461         END-IF                                                    GBIFPGM 
04462      ELSE                                                         GBIFPGM 
04463      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'UCCA'                     GBIFPGM 
04464         IF WS-PROCESS-CON                                         GBIFPGM 
04465            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04466               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04467                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04468               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04469                                 TO  GCBH-UCF-COPAY-OTH            GBIFPGM 
04470            ELSE                                                   GBIFPGM 
04471               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04472                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04473               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04474                              TO  GCBH-UCF-COPAY-OTH               GBIFPGM 
04475         END-IF.                                                   GBIFPGM 
04476                                                                   GBIFPGM 
04477  4094A-EXIT.                                                      GBIFPGM 
04478      EXIT.                                                        GBIFPGM 
04479 /                                                                 GBIFPGM 
04480 ******************************************************************GBIFPGM 
04481 *                                                                 GBIFPGM 
04482 *    OUTPATIENT HOSPITAL  COPAY                                   GBIFPGM 
04483 *                                                                 GBIFPGM 
04484 ******************************************************************GBIFPGM 
04485  4095-BLD-OP-HOSP-COPAY.                                          GBIFPGM 
04486 *MQ 10/09/03                                                      GBIFPGM 
04487                                                                   GBIFPGM 
04488      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'OPFI'                    GBIFPGM 
04489         IF WS-PROCESS-CON                                         GBIFPGM 
04490            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04491               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04492                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04493               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04494                              TO  GCBH-OP-HOSP-COPAY-IN            GBIFPGM 
04495               MOVE 'Y'                TO WS-OPFI-VALQUAL5-SW      GBIFPGM 
04496            ELSE                                                   GBIFPGM 
04497               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04498                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04499               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04500                              TO  GCBH-OP-HOSP-COPAY-IN            GBIFPGM 
04501            END-IF                                                 GBIFPGM 
04502         END-IF                                                    GBIFPGM 
04503      ELSE                                                         GBIFPGM 
04504      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'OPFO'                    GBIFPGM 
04505         IF WS-PROCESS-GRP                                         GBIFPGM 
04506            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04507               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04508                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04509               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04510                              TO  GCBH-OP-HOSP-COPAY-OUT           GBIFPGM 
04511               MOVE 'Y'                TO WS-OPFO-VALQUAL5-SW      GBIFPGM 
04512            ELSE                                                   GBIFPGM 
04513               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04514                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04515               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04516                              TO  GCBH-OP-HOSP-COPAY-OUT           GBIFPGM 
04517            END-IF                                                 GBIFPGM 
04518         END-IF                                                    GBIFPGM 
04519      ELSE                                                         GBIFPGM 
04520      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'OPFA'                    GBIFPGM 
04521         IF WS-PROCESS-CON                                         GBIFPGM 
04522            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04523               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04524                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04525               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04526                                 TO  GCBH-OP-HOSP-COPAY-OTH        GBIFPGM 
04527               MOVE 'Y'                   TO WS-OPFA-VALQUAL5-SW   GBIFPGM 
04528            ELSE                                                   GBIFPGM 
04529               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04530                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04531               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04532                                 TO  GCBH-OP-HOSP-COPAY-OTH        GBIFPGM 
04533            END-IF                                                 GBIFPGM 
04534         END-IF.                                                   GBIFPGM 
04535                                                                   GBIFPGM 
04536  4095-EXIT.                                                       GBIFPGM 
04537      EXIT.                                                        GBIFPGM 
04538 /                                                                 GBIFPGM 
04539  4095A-BLD-OP-HOSP-COPAY.                                         GBIFPGM 
04540                                                                   GBIFPGM 
04541 *MQ 10/09/03                                                      GBIFPGM 
04542      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'OPFI'                     GBIFPGM 
04543         IF WS-PROCESS-CON                                         GBIFPGM 
04544            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04545               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04546                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04547               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04548                              TO  GCBH-OP-HOSP-COPAY-IN            GBIFPGM 
04549            ELSE                                                   GBIFPGM 
04550               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04551                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04552               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04553                              TO  GCBH-OP-HOSP-COPAY-IN            GBIFPGM 
04554            END-IF                                                 GBIFPGM 
04555         END-IF                                                    GBIFPGM 
04556      ELSE                                                         GBIFPGM 
04557      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'OPFO'                     GBIFPGM 
04558         IF WS-PROCESS-GRP                                         GBIFPGM 
04559            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04560               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04561                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04562               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04563                              TO  GCBH-OP-HOSP-COPAY-OUT           GBIFPGM 
04564            ELSE                                                   GBIFPGM 
04565               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04566                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04567               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04568                              TO  GCBH-OP-HOSP-COPAY-OUT           GBIFPGM 
04569            END-IF                                                 GBIFPGM 
04570         END-IF                                                    GBIFPGM 
04571      ELSE                                                         GBIFPGM 
04572      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'OPFA'                     GBIFPGM 
04573         IF WS-PROCESS-CON                                         GBIFPGM 
04574            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04575               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04576                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04577               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04578                                 TO  GCBH-OP-HOSP-COPAY-OTH        GBIFPGM 
04579            ELSE                                                   GBIFPGM 
04580               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04581                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04582               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04583                                 TO  GCBH-OP-HOSP-COPAY-OTH        GBIFPGM 
04584         END-IF.                                                   GBIFPGM 
04585                                                                   GBIFPGM 
04586  4095A-EXIT.                                                      GBIFPGM 
04587      EXIT.                                                        GBIFPGM 
04588 /                                                                 GBIFPGM 
04589 ******************************************************************GBIFPGM 
04590 *                                                                 GBIFPGM 
04591 *    URGENT CARE PROFESSIONAL COPAY                               GBIFPGM 
04592 *                                                                 GBIFPGM 
04593 ******************************************************************GBIFPGM 
04594  4096-BLD-UCP-COPAY.                                              GBIFPGM 
04595 *MQ 10/09/03                                                      GBIFPGM 
04596                                                                   GBIFPGM 
04597      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'UCPI'                    GBIFPGM 
04598         IF WS-PROCESS-CON                                         GBIFPGM 
04599            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04600               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04601                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04602               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04603                              TO  GCBH-UCP-COPAY-IN                GBIFPGM 
04604               MOVE 'Y'                TO WS-UCPI-VALQUAL5-SW      GBIFPGM 
04605            ELSE                                                   GBIFPGM 
04606               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04607                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04608               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04609                              TO  GCBH-UCP-COPAY-IN                GBIFPGM 
04610            END-IF                                                 GBIFPGM 
04611         END-IF                                                    GBIFPGM 
04612      ELSE                                                         GBIFPGM 
04613      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'UCPO'                    GBIFPGM 
04614         IF WS-PROCESS-GRP                                         GBIFPGM 
04615            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04616               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04617                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04618               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04619                              TO  GCBH-UCP-COPAY-OUT               GBIFPGM 
04620               MOVE 'Y'                TO WS-UCPO-VALQUAL5-SW      GBIFPGM 
04621            ELSE                                                   GBIFPGM 
04622               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04623                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04624               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04625                              TO  GCBH-UCP-COPAY-OUT               GBIFPGM 
04626            END-IF                                                 GBIFPGM 
04627         END-IF                                                    GBIFPGM 
04628      ELSE                                                         GBIFPGM 
04629      IF GAF-COPAY-ACCUMID (GAF-INDEX) = 'UCPA'                    GBIFPGM 
04630         IF WS-PROCESS-CON                                         GBIFPGM 
04631            IF GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'         GBIFPGM 
04632               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04633                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04634               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04635                                 TO  GCBH-UCP-COPAY-OTH            GBIFPGM 
04636               MOVE 'Y'                   TO WS-UCPA-VALQUAL5-SW   GBIFPGM 
04637            ELSE                                                   GBIFPGM 
04638               MOVE GAF-COPAY-VALUE-LIMIT (GAF-INDEX)              GBIFPGM 
04639                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04640               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04641                                 TO  GCBH-UCP-COPAY-OTH            GBIFPGM 
04642            END-IF                                                 GBIFPGM 
04643         END-IF.                                                   GBIFPGM 
04644                                                                   GBIFPGM 
04645  4096-EXIT.                                                       GBIFPGM 
04646      EXIT.                                                        GBIFPGM 
04647 /                                                                 GBIFPGM 
04648  4096A-BLD-UCP-COPAY.                                             GBIFPGM 
04649                                                                   GBIFPGM 
04650 *MQ 10/09/03                                                      GBIFPGM 
04651      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'UCPI'                     GBIFPGM 
04652         IF WS-PROCESS-CON                                         GBIFPGM 
04653            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04654               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04655                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04656               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04657                              TO  GCBH-UCP-COPAY-IN                GBIFPGM 
04658            ELSE                                                   GBIFPGM 
04659               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04660                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04661               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04662                              TO  GCBH-UCP-COPAY-IN                GBIFPGM 
04663            END-IF                                                 GBIFPGM 
04664         END-IF                                                    GBIFPGM 
04665      ELSE                                                         GBIFPGM 
04666      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'UCPO'                     GBIFPGM 
04667         IF WS-PROCESS-GRP                                         GBIFPGM 
04668            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04669               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04670                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04671               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04672                              TO GCBH-UCP-COPAY-OUT                GBIFPGM 
04673            ELSE                                                   GBIFPGM 
04674               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04675                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04676               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04677                              TO  GCBH-UCP-COPAY-OUT               GBIFPGM 
04678            END-IF                                                 GBIFPGM 
04679         END-IF                                                    GBIFPGM 
04680      ELSE                                                         GBIFPGM 
04681      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'UCPA'                     GBIFPGM 
04682         IF WS-PROCESS-CON                                         GBIFPGM 
04683            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)  = '5'         GBIFPGM 
04684               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04685                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04686               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
04687                                 TO  GCBH-UCP-COPAY-OTH            GBIFPGM 
04688            ELSE                                                   GBIFPGM 
04689               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04690                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04691               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
04692                                 TO  GCBH-UCP-COPAY-OTH            GBIFPGM 
04693            END-IF                                                 GBIFPGM 
04694         END-IF.                                                   GBIFPGM 
04695                                                                   GBIFPGM 
04696  4096A-EXIT.                                                      GBIFPGM 
04697      EXIT.                                                        GBIFPGM 
04698 /                                                                 GBIFPGM 
04699 ******************************************************************GBIFPGM 
04700 *                                                                 GBIFPGM 
04701 *    HOSPITAL PAYMENT LEVEL                                       GBIFPGM 
04702 *                                                                 GBIFPGM 
04703 ******************************************************************GBIFPGM 
04704  4100-BLD-HOSP-PMT-LVL.                                           GBIFPGM 
04705                                                                   GBIFPGM 
04706      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'IPHI'                    GBIFPGM 
04707         IF WS-PROCESS-CON                                         GBIFPGM 
04708            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04709                                       TO WS-PERCENT-VAL           GBIFPGM 
04710            MOVE WS-PERCENT            TO GCBH-HOSP-PMT-LVL-IN     GBIFPGM 
04711         END-IF                                                    GBIFPGM 
04712      ELSE                                                         GBIFPGM 
04713      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'IPHO'                    GBIFPGM 
04714         IF WS-PROCESS-GRP                                         GBIFPGM 
04715            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04716                                       TO WS-PERCENT-VAL           GBIFPGM 
04717            MOVE WS-PERCENT            TO GCBH-HOSP-PMT-LVL-OUT    GBIFPGM 
04718         END-IF                                                    GBIFPGM 
04719      ELSE                                                         GBIFPGM 
04720      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'IPHA'                    GBIFPGM 
04721         IF WS-PROCESS-CON                                         GBIFPGM 
04722            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04723                                       TO WS-PERCENT-VAL           GBIFPGM 
04724            MOVE WS-PERCENT            TO GCBH-HOSP-PMT-LVL-OTH.   GBIFPGM 
04725                                                                   GBIFPGM 
04726  4100-EXIT.                                                       GBIFPGM 
04727      EXIT.                                                        GBIFPGM 
04728 /                                                                 GBIFPGM 
04729 ******************************************************************GBIFPGM 
04730 *                                                                 GBIFPGM 
04731 *    PER ADMISSION DEDUCTIBLE                                     GBIFPGM 
04732 *                                                                 GBIFPGM 
04733 ******************************************************************GBIFPGM 
04734  4110-BLD-PER-ADM-DED.                                            GBIFPGM 
04735                                                                   GBIFPGM 
04736      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'PADI'                     GBIFPGM 
04737         IF WS-PROCESS-CON                                         GBIFPGM 
04738            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04739               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04740                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04741               MOVE WS-VALUE-LIMIT-S      TO GCBH-PER-ADM-DED-IN   GBIFPGM 
04742            ELSE                                                   GBIFPGM 
04743               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04744                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04745               MOVE WS-VALUE-LIMIT        TO GCBH-PER-ADM-DED-IN   GBIFPGM 
04746            END-IF                                                 GBIFPGM 
04747         END-IF                                                    GBIFPGM 
04748      ELSE                                                         GBIFPGM 
04749      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'PADO'                     GBIFPGM 
04750         IF WS-PROCESS-GRP                                         GBIFPGM 
04751            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
04752               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04753                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
04754               MOVE WS-VALUE-LIMIT-S      TO GCBH-PER-ADM-DED-OUT  GBIFPGM 
04755 *JP 3/21/03                                                       GBIFPGM 
04756               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04757                                          TO  WS-WRK-VAL-1         GBIFPGM 
04758               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 - 25.00         GBIFPGM 
04759               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
04760               MOVE WS-WRK-VAL-BUX TO GCBH-PER-ADM-DED-OUT-UTL     GBIFPGM 
04761            ELSE                                                   GBIFPGM 
04762               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
04763                                          TO  WS-VALUE-LIMIT       GBIFPGM 
04764               MOVE WS-VALUE-LIMIT        TO GCBH-PER-ADM-DED-OUT  GBIFPGM 
04765            END-IF                                                 GBIFPGM 
04766         END-IF                                                    GBIFPGM 
04767      ELSE                                                         GBIFPGM 
04768      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'PADA'                     GBIFPGM 
04769         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
04770            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
04771                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
04772            MOVE WS-VALUE-LIMIT-S      TO GCBH-PER-ADM-DED-OTH     GBIFPGM 
04773         ELSE                                                      GBIFPGM 
04774            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
04775                                       TO  WS-VALUE-LIMIT          GBIFPGM 
04776            MOVE WS-VALUE-LIMIT        TO GCBH-PER-ADM-DED-OTH     GBIFPGM 
04777         END-IF.                                                   GBIFPGM 
04778                                                                   GBIFPGM 
04779  4110-EXIT.                                                       GBIFPGM 
06302      EXIT.                                                        GBIFPGM 
06303 /                                                                 GBIFPGM 
06304 *MQ 05/03/05                                                      GBIFPGM 
06305 ******************************************************************GBIFPGM 
06306 *                                                                 GBIFPGM 
06307 *    PER ADMISSION DEDUCTIBLE MAX                                 GBIFPGM 
06308 *                                                                 GBIFPGM 
06309 ******************************************************************GBIFPGM 
06310  4115-BLD-PER-ADM-DED-MAX.                                        GBIFPGM 
06311                                                                   GBIFPGM 
06312      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'PDMI'                     GBIFPGM 
06313         IF WS-PROCESS-CON                                         GBIFPGM 
06314            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
06315               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
06316                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
06317               MOVE WS-VALUE-LIMIT-S  TO GCBH-PER-ADM-DED-MAX-IN   GBIFPGM 
06318            ELSE                                                   GBIFPGM 
06319               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
06320                                      TO  WS-VALUE-LIMIT           GBIFPGM 
06321               MOVE WS-VALUE-LIMIT    TO GCBH-PER-ADM-DED-MAX-IN   GBIFPGM 
06322            END-IF                                                 GBIFPGM 
06323         END-IF                                                    GBIFPGM 
06324      ELSE                                                         GBIFPGM 
06325      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'PDMO'                     GBIFPGM 
06326         IF WS-PROCESS-GRP                                         GBIFPGM 
06327            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
06328               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
06329                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
06330               MOVE WS-VALUE-LIMIT-S  TO GCBH-PER-ADM-DED-MAX-OUT  GBIFPGM 
06331 *JP 3/21/03                                                       GBIFPGM 
06332               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
06333                                          TO  WS-WRK-VAL-1         GBIFPGM 
06334               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 - 25.00         GBIFPGM 
06335               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
06336               MOVE WS-WRK-VAL-BUX TO                              GBIFPGM 
06337                    GCBH-PER-ADM-DED-OUT-MAX-UTL                   GBIFPGM 
06338            ELSE                                                   GBIFPGM 
06339               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
06340                                   TO  WS-VALUE-LIMIT              GBIFPGM 
06341               MOVE WS-VALUE-LIMIT TO GCBH-PER-ADM-DED-MAX-OUT     GBIFPGM 
06342            END-IF                                                 GBIFPGM 
06343         END-IF                                                    GBIFPGM 
06344      ELSE                                                         GBIFPGM 
06345      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'PDMA'                     GBIFPGM 
06346         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
06347            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
06348                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
06349            MOVE WS-VALUE-LIMIT-S  TO  GCBH-PER-ADM-DED-MAX-OTH    GBIFPGM 
06350         ELSE                                                      GBIFPGM 
06351            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
06352                                       TO  WS-VALUE-LIMIT          GBIFPGM 
06353            MOVE WS-VALUE-LIMIT    TO  GCBH-PER-ADM-DED-MAX-OTH    GBIFPGM 
06354         END-IF.                                                   GBIFPGM 
06355                                                                   GBIFPGM 
06356  4115-EXIT.                                                       GBIFPGM 
04780      EXIT.                                                        GBIFPGM 
04781 /                                                                 GBIFPGM 
04782 ******************************************************************GBIFPGM 
04783 *                                                                 GBIFPGM 
04784 *    OUTPATIENT SURGERY HOSPITAL PAYMENT LEVEL                    GBIFPGM 
04785 *                                                                 GBIFPGM 
04786 ******************************************************************GBIFPGM 
04787  4120-BLD-OS-HOS-PMT-LVL.                                         GBIFPGM 
04788                                                                   GBIFPGM 
04789      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSIH'                    GBIFPGM 
04790         IF WS-PROCESS-CON                                         GBIFPGM 
04791            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04792                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04793            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04794              TO WS-PERCENT-VAL                                    GBIFPGM 
04795            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04796                                       TO WS-PERCENT-VAL           GBIFPGM 
04797            MOVE WS-PERCENT            TO GCBH-OS-HOSP-PMT-LVL-IN  GBIFPGM 
04798         END-IF                                                    GBIFPGM 
04799      END-IF                                                       GBIFPGM 
04800      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSOH'                    GBIFPGM 
04801         IF WS-PROCESS-GRP                                         GBIFPGM 
04802            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04803                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04804            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04805                                       TO WS-PERCENT-VAL           GBIFPGM 
04806            MOVE WS-PERCENT            TO GCBH-OS-HOSP-PMT-LVL-OUT GBIFPGM 
04807         END-IF                                                    GBIFPGM 
04808      END-IF                                                       GBIFPGM 
04809      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSAH'                    GBIFPGM 
04810         IF WS-PROCESS-CON                                         GBIFPGM 
04811            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04812                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04813            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04814                                          TO WS-PERCENT-VAL        GBIFPGM 
04815            MOVE WS-PERCENT            TO GCBH-OS-HOSP-PMT-LVL-OTH GBIFPGM 
04816         END-IF                                                    GBIFPGM 
04817      END-IF.                                                      GBIFPGM 
04818                                                                   GBIFPGM 
04819  4120-EXIT.                                                       GBIFPGM 
04820      EXIT.                                                        GBIFPGM 
04821 /                                                                 GBIFPGM 
04822 ******************************************************************GBIFPGM 
04823 *                                                                 GBIFPGM 
04824 *    OUTPATIENT SURGERY PROFESSIONAL PAYMENT LEVEL                GBIFPGM 
04825 *                                                                 GBIFPGM 
04826 ******************************************************************GBIFPGM 
04827  4130-BLD-OS-PRF-PMT-LVL.                                         GBIFPGM 
04828                                                                   GBIFPGM 
04829      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSIP'                    GBIFPGM 
04830         IF WS-PROCESS-CON                                         GBIFPGM 
04831            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04832                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04833            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04834              TO WS-PERCENT-VAL                                    GBIFPGM 
04835            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04836                                       TO WS-PERCENT-VAL           GBIFPGM 
04837            MOVE WS-PERCENT            TO GCBH-OS-PROF-PMT-LVL-IN  GBIFPGM 
04838         END-IF                                                    GBIFPGM 
04839      END-IF                                                       GBIFPGM 
04840      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSOP'                    GBIFPGM 
04841         IF WS-PROCESS-GRP                                         GBIFPGM 
04842            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04843                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04844            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04845                                       TO WS-PERCENT-VAL           GBIFPGM 
04846            MOVE WS-PERCENT            TO GCBH-OS-PROF-PMT-LVL-OUT GBIFPGM 
04847         END-IF                                                    GBIFPGM 
04848      END-IF                                                       GBIFPGM 
04849      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSAP'                    GBIFPGM 
04850         IF WS-PROCESS-CON                                         GBIFPGM 
04851            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04852                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04853            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04854                                              TO WS-PERCENT-VAL    GBIFPGM 
04855            MOVE WS-PERCENT            TO GCBH-OS-PROF-PMT-LVL-OTH GBIFPGM 
04856         END-IF                                                    GBIFPGM 
04857      END-IF.                                                      GBIFPGM 
04858                                                                   GBIFPGM 
04859  4130-EXIT.                                                       GBIFPGM 
04860      EXIT.                                                        GBIFPGM 
04861 /                                                                 GBIFPGM 
04862 *MQ 10/03                                                         GBIFPGM 
04863 ******************************************************************GBIFPGM 
04864 *                                                                 GBIFPGM 
04865 *    OUTPATIENT SURGERY B/C AND B/S PAYMENT LEVEL                 GBIFPGM 
04866 *                                                                 GBIFPGM 
04867 ******************************************************************GBIFPGM 
04868  4135-BLD-OS-BCBS-PMT-LVL.                                        GBIFPGM 
04869                                                                   GBIFPGM 
04870      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSPI'                    GBIFPGM 
04871         IF WS-PROCESS-CON                                         GBIFPGM 
04872            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04873                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04874            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04875              TO WS-PERCENT-VAL                                    GBIFPGM 
04876            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04877                                       TO WS-PERCENT-VAL           GBIFPGM 
04878            MOVE WS-PERCENT            TO GCBH-OS-BCBS-PMT-LVL-IN  GBIFPGM 
04879         END-IF                                                    GBIFPGM 
04880      END-IF                                                       GBIFPGM 
04881      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSPO'                    GBIFPGM 
04882         IF WS-PROCESS-GRP                                         GBIFPGM 
04883            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04884                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04885            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04886                                       TO WS-PERCENT-VAL           GBIFPGM 
04887            MOVE WS-PERCENT            TO GCBH-OS-BCBS-PMT-LVL-OUT GBIFPGM 
04888         END-IF                                                    GBIFPGM 
04889      END-IF                                                       GBIFPGM 
04890      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OSPA'                    GBIFPGM 
04891         IF WS-PROCESS-CON                                         GBIFPGM 
04892            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04893                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04894            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04895                                              TO WS-PERCENT-VAL    GBIFPGM 
04896            MOVE WS-PERCENT            TO GCBH-OS-BCBS-PMT-LVL-OTH GBIFPGM 
04897         END-IF                                                    GBIFPGM 
04898      END-IF.                                                      GBIFPGM 
04899                                                                   GBIFPGM 
04900  4135-EXIT.                                                       GBIFPGM 
04901      EXIT.                                                        GBIFPGM 
04902 /                                                                 GBIFPGM 
04903 ******************************************************************GBIFPGM 
04904 *                                                                 GBIFPGM 
04905 *    OUTPATIENT DIAGNOSTIC HOSPITAL PAYMENT LEVEL                 GBIFPGM 
04906 *                                                                 GBIFPGM 
04907 ******************************************************************GBIFPGM 
04908  4140-BLD-OD-HOS-PMT-LVL.                                         GBIFPGM 
04909                                                                   GBIFPGM 
04910      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ODIH'                    GBIFPGM 
04911         IF WS-PROCESS-CON                                         GBIFPGM 
04912            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04913                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04914            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04915              TO WS-PERCENT-VAL                                    GBIFPGM 
04916            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04917                                       TO WS-PERCENT-VAL           GBIFPGM 
04918            MOVE WS-PERCENT            TO GCBH-OD-HOSP-PMT-LVL-IN  GBIFPGM 
04919         END-IF                                                    GBIFPGM 
04920      END-IF                                                       GBIFPGM 
04921      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ODOH'                    GBIFPGM 
04922         IF WS-PROCESS-GRP                                         GBIFPGM 
04923            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04924                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04925            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04926                                       TO WS-PERCENT-VAL           GBIFPGM 
04927            MOVE WS-PERCENT            TO GCBH-OD-HOSP-PMT-LVL-OUT GBIFPGM 
04928         END-IF                                                    GBIFPGM 
04929      END-IF                                                       GBIFPGM 
04930      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ODAH'                    GBIFPGM 
04931         IF WS-PROCESS-CON                                         GBIFPGM 
04932            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04933                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04934            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04935                                          TO WS-PERCENT-VAL        GBIFPGM 
04936            MOVE WS-PERCENT            TO GCBH-OD-HOSP-PMT-LVL-OTH GBIFPGM 
04937         END-IF                                                    GBIFPGM 
04938      END-IF.                                                      GBIFPGM 
04939                                                                   GBIFPGM 
04940  4140-EXIT.                                                       GBIFPGM 
04941      EXIT.                                                        GBIFPGM 
04942 /                                                                 GBIFPGM 
04943 ******************************************************************GBIFPGM 
04944 *                                                                 GBIFPGM 
04945 *    OUTPATIENT DIAGNOSTIC PROFESSIONAL PAYMENT LEVEL             GBIFPGM 
04946 *                                                                 GBIFPGM 
04947 ******************************************************************GBIFPGM 
04948  4150-BLD-OD-PRF-PMT-LVL.                                         GBIFPGM 
04949                                                                   GBIFPGM 
04950      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ODIP'                    GBIFPGM 
04951         IF WS-PROCESS-CON                                         GBIFPGM 
04952            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04953                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04954            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04955              TO WS-PERCENT-VAL                                    GBIFPGM 
04956            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04957                                       TO WS-PERCENT-VAL           GBIFPGM 
04958            MOVE WS-PERCENT            TO GCBH-OD-PROF-PMT-LVL-IN  GBIFPGM 
04959         END-IF                                                    GBIFPGM 
04960      END-IF                                                       GBIFPGM 
04961      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ODOP'                    GBIFPGM 
04962         IF WS-PROCESS-GRP                                         GBIFPGM 
04963            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04964                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04965            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04966                                       TO WS-PERCENT-VAL           GBIFPGM 
04967            MOVE WS-PERCENT            TO GCBH-OD-PROF-PMT-LVL-OUT GBIFPGM 
04968         END-IF                                                    GBIFPGM 
04969      END-IF                                                       GBIFPGM 
04970      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ODAP'                    GBIFPGM 
04971         IF WS-PROCESS-CON                                         GBIFPGM 
04972            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04973                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04974            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04975                                          TO WS-PERCENT-VAL        GBIFPGM 
04976            MOVE WS-PERCENT            TO GCBH-OD-PROF-PMT-LVL-OTH GBIFPGM 
04977         END-IF                                                    GBIFPGM 
04978      END-IF.                                                      GBIFPGM 
04979                                                                   GBIFPGM 
04980  4150-EXIT.                                                       GBIFPGM 
04981      EXIT.                                                        GBIFPGM 
04982 /                                                                 GBIFPGM 
04983 ******************************************************************GBIFPGM 
04984 *                                                                 GBIFPGM 
04985 *    EMERGENCY ACCIDENT CARE HOSPITAL PAYMENT LEVEL               GBIFPGM 
04986 *                                                                 GBIFPGM 
04987 ******************************************************************GBIFPGM 
04988  4160-BLD-EMER-ACID-HOSP.                                         GBIFPGM 
04989                                                                   GBIFPGM 
04990      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAHI'                    GBIFPGM 
04991         IF WS-PROCESS-CON                                         GBIFPGM 
04992            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
04993                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
04994            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04995              TO WS-PERCENT-VAL                                    GBIFPGM 
04996            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
04997                                       TO WS-PERCENT-VAL           GBIFPGM 
04998            MOVE WS-PERCENT            TO GCBH-EAC-HOSP-PMT-LVL-IN GBIFPGM 
04999         END-IF                                                    GBIFPGM 
05000      END-IF                                                       GBIFPGM 
05001      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAHO'                    GBIFPGM 
05002         IF WS-PROCESS-GRP                                         GBIFPGM 
05003            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05004                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05005            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05006                                       TO WS-PERCENT-VAL           GBIFPGM 
05007            MOVE WS-PERCENT            TO GCBH-EAC-HOSP-PMT-LVL-OUTGBIFPGM 
05008         END-IF                                                    GBIFPGM 
05009      END-IF                                                       GBIFPGM 
05010      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAHA'                    GBIFPGM 
05011         IF WS-PROCESS-CON                                         GBIFPGM 
05012            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05013                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05014            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05015                                          TO WS-PERCENT-VAL        GBIFPGM 
05016            MOVE WS-PERCENT           TO GCBH-EAC-HOSP-PMT-LVL-OTH GBIFPGM 
05017         END-IF                                                    GBIFPGM 
05018      END-IF.                                                      GBIFPGM 
05019                                                                   GBIFPGM 
05020  4160-EXIT.                                                       GBIFPGM 
05021      EXIT.                                                        GBIFPGM 
05022 /                                                                 GBIFPGM 
05023 ******************************************************************GBIFPGM 
05024 *                                                                 GBIFPGM 
05025 *    EMERGENCY ACCIDENT CARE PROFESSIONAL PAYMENT LEVEL           GBIFPGM 
05026 *                                                                 GBIFPGM 
05027 ******************************************************************GBIFPGM 
05028  4170-BLD-EMER-ACID-PROF.                                         GBIFPGM 
05029                                                                   GBIFPGM 
05030      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAPI'                    GBIFPGM 
05031         IF WS-PROCESS-CON                                         GBIFPGM 
05032            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05033                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05034            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05035              TO WS-PERCENT-VAL                                    GBIFPGM 
05036            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05037                                       TO WS-PERCENT-VAL           GBIFPGM 
05038            MOVE WS-PERCENT            TO GCBH-EAC-PROF-PMT-LVL-IN GBIFPGM 
05039         END-IF                                                    GBIFPGM 
05040      END-IF                                                       GBIFPGM 
05041      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAPO'                    GBIFPGM 
05042         IF WS-PROCESS-GRP                                         GBIFPGM 
05043            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05044                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05045            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05046                                       TO WS-PERCENT-VAL           GBIFPGM 
05047            MOVE WS-PERCENT            TO GCBH-EAC-PROF-PMT-LVL-OUTGBIFPGM 
05048         END-IF                                                    GBIFPGM 
05049      END-IF                                                       GBIFPGM 
05050      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAPA'                    GBIFPGM 
05051         IF WS-PROCESS-CON                                         GBIFPGM 
05052            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05053                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05054            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05055                                          TO WS-PERCENT-VAL        GBIFPGM 
05056            MOVE WS-PERCENT          TO GCBH-EAC-PROF-PMT-LVL-OTH  GBIFPGM 
05057         END-IF                                                    GBIFPGM 
05058      END-IF.                                                      GBIFPGM 
05059                                                                   GBIFPGM 
05060  4170-EXIT.                                                       GBIFPGM 
05061      EXIT.                                                        GBIFPGM 
05062 /                                                                 GBIFPGM 
05063 *MQ 10/03                                                         GBIFPGM 
05064 ******************************************************************GBIFPGM 
05065 *                                                                 GBIFPGM 
05066 *    EAC B/C AND B/S PAYMENT LEVEL                                GBIFPGM 
05067 *                                                                 GBIFPGM 
05068 ******************************************************************GBIFPGM 
05069  4175-BLD-EAC-BCBS-PMT-LVL.                                       GBIFPGM 
05070                                                                   GBIFPGM 
05071      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EACI'                    GBIFPGM 
05072         IF WS-PROCESS-CON                                         GBIFPGM 
05073            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05074                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05075            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05076              TO WS-PERCENT-VAL                                    GBIFPGM 
05077            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05078                                       TO WS-PERCENT-VAL           GBIFPGM 
05079            MOVE WS-PERCENT            TO GCBH-EAC-BCBS-PMT-LVL-IN GBIFPGM 
05080         END-IF                                                    GBIFPGM 
05081      END-IF                                                       GBIFPGM 
05082      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EACO'                    GBIFPGM 
05083         IF WS-PROCESS-GRP                                         GBIFPGM 
05084            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05085                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05086            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05087                                       TO WS-PERCENT-VAL           GBIFPGM 
05088            MOVE WS-PERCENT            TO GCBH-EAC-BCBS-PMT-LVL-OUTGBIFPGM 
05089         END-IF                                                    GBIFPGM 
05090      END-IF                                                       GBIFPGM 
05091      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EACA'                    GBIFPGM 
05092         IF WS-PROCESS-CON                                         GBIFPGM 
05093            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05094                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05095            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05096                                          TO WS-PERCENT-VAL        GBIFPGM 
05097            MOVE WS-PERCENT          TO GCBH-EAC-BCBS-PMT-LVL-OTH  GBIFPGM 
05098         END-IF                                                    GBIFPGM 
05099      END-IF.                                                      GBIFPGM 
05100                                                                   GBIFPGM 
05101  4175-EXIT.                                                       GBIFPGM 
05102      EXIT.                                                        GBIFPGM 
05103 /                                                                 GBIFPGM 
05104 ******************************************************************GBIFPGM 
05105 *                                                                 GBIFPGM 
05106 *    EMERGENCY MEDICAL CARE HOSPITAL PAYMENT LEVEL                GBIFPGM 
05107 *                                                                 GBIFPGM 
05108 ******************************************************************GBIFPGM 
05109  4180-BLD-EMC-HOSP-PMT.                                           GBIFPGM 
05110                                                                   GBIFPGM 
05111      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMHI'                    GBIFPGM 
05112         IF WS-PROCESS-CON                                         GBIFPGM 
05113            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05114                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05115            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05116              TO WS-PERCENT-VAL                                    GBIFPGM 
05117            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05118                                       TO WS-PERCENT-VAL           GBIFPGM 
05119            MOVE WS-PERCENT            TO GCBH-EAC-PROF-PMT-LVL-IN GBIFPGM 
05120         END-IF                                                    GBIFPGM 
05121      END-IF                                                       GBIFPGM 
05122      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMHO'                    GBIFPGM 
05123         IF WS-PROCESS-GRP                                         GBIFPGM 
05124            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05125                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05126            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05127                                       TO WS-PERCENT-VAL           GBIFPGM 
05128            MOVE WS-PERCENT            TO GCBH-EMC-HOSP-PMT-LVL-OUTGBIFPGM 
05129         END-IF                                                    GBIFPGM 
05130      END-IF                                                       GBIFPGM 
05131      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMHA'                    GBIFPGM 
05132         IF WS-PROCESS-CON                                         GBIFPGM 
05133            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05134                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05135            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05136                                          TO WS-PERCENT-VAL        GBIFPGM 
05137            MOVE WS-PERCENT          TO GCBH-EMC-HOSP-PMT-LVL-OTH  GBIFPGM 
05138         END-IF                                                    GBIFPGM 
05139      END-IF.                                                      GBIFPGM 
05140                                                                   GBIFPGM 
05141  4180-EXIT.                                                       GBIFPGM 
05142      EXIT.                                                        GBIFPGM 
05143 /                                                                 GBIFPGM 
05144 ******************************************************************GBIFPGM 
05145 *                                                                 GBIFPGM 
05146 *    EMERGENCY MEDICAL CARE PROFESSIONAL PAYMENT LEVEL            GBIFPGM 
05147 *                                                                 GBIFPGM 
05148 ******************************************************************GBIFPGM 
05149  4190-BLD-EMC-PROF-PMT.                                           GBIFPGM 
05150                                                                   GBIFPGM 
05151      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMPI'                    GBIFPGM 
05152         IF WS-PROCESS-CON                                         GBIFPGM 
05153            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05154                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05155            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05156              TO WS-PERCENT-VAL                                    GBIFPGM 
05157            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05158                                       TO WS-PERCENT-VAL           GBIFPGM 
05159            MOVE WS-PERCENT            TO GCBH-EMC-PROF-PMT-LVL-IN GBIFPGM 
05160         END-IF                                                    GBIFPGM 
05161      END-IF                                                       GBIFPGM 
05162      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMPO'                    GBIFPGM 
05163         IF WS-PROCESS-GRP                                         GBIFPGM 
05164            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05165                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05166            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05167                                       TO WS-PERCENT-VAL           GBIFPGM 
05168            MOVE WS-PERCENT            TO GCBH-EMC-PROF-PMT-LVL-OUTGBIFPGM 
05169         END-IF                                                    GBIFPGM 
05170      END-IF                                                       GBIFPGM 
05171      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMPA'                    GBIFPGM 
05172         IF WS-PROCESS-CON                                         GBIFPGM 
05173            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05174                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05175            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05176                                          TO WS-PERCENT-VAL        GBIFPGM 
05177            MOVE WS-PERCENT          TO GCBH-EMC-PROF-PMT-LVL-OTH  GBIFPGM 
05178         END-IF                                                    GBIFPGM 
05179      END-IF.                                                      GBIFPGM 
05180                                                                   GBIFPGM 
05181  4190-EXIT.                                                       GBIFPGM 
05182      EXIT.                                                        GBIFPGM 
05183 /                                                                 GBIFPGM 
05184 *MQ 10/03                                                         GBIFPGM 
05185 ******************************************************************GBIFPGM 
05186 *                                                                 GBIFPGM 
05187 *    EMC B/C AND B/S PAYMENT LEVEL                                GBIFPGM 
05188 *                                                                 GBIFPGM 
05189 ******************************************************************GBIFPGM 
05190  4195-BLD-EMC-BCBS-PMT-LVL.                                       GBIFPGM 
05191                                                                   GBIFPGM 
05192      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMCI'                    GBIFPGM 
05193         IF WS-PROCESS-CON                                         GBIFPGM 
05194            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05195                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05196            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05197              TO WS-PERCENT-VAL                                    GBIFPGM 
05198            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05199                                       TO WS-PERCENT-VAL           GBIFPGM 
05200            MOVE WS-PERCENT            TO GCBH-EMC-BCBS-PMT-LVL-IN GBIFPGM 
05201         END-IF                                                    GBIFPGM 
05202      END-IF                                                       GBIFPGM 
05203      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMCO'                    GBIFPGM 
05204         IF WS-PROCESS-GRP                                         GBIFPGM 
05205            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05206                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05207            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05208                                       TO WS-PERCENT-VAL           GBIFPGM 
05209            MOVE WS-PERCENT            TO GCBH-EMC-BCBS-PMT-LVL-OUTGBIFPGM 
05210         END-IF                                                    GBIFPGM 
05211      END-IF                                                       GBIFPGM 
05212      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMCA'                    GBIFPGM 
05213         IF WS-PROCESS-CON                                         GBIFPGM 
05214            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05215                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05216            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05217                                          TO WS-PERCENT-VAL        GBIFPGM 
05218            MOVE WS-PERCENT          TO GCBH-EMC-BCBS-PMT-LVL-OTH  GBIFPGM 
05219         END-IF                                                    GBIFPGM 
05220      END-IF.                                                      GBIFPGM 
05221                                                                   GBIFPGM 
05222  4195-EXIT.                                                       GBIFPGM 
05223      EXIT.                                                        GBIFPGM 
05224 /                                                                 GBIFPGM 
05225 *MQ 10/03                                                         GBIFPGM 
05226 ******************************************************************GBIFPGM 
05227 *                                                                 GBIFPGM 
05228 *    EAC/EMC BLUE CROSS PAYMENT LEVEL                             GBIFPGM 
05229 *                                                                 GBIFPGM 
05230 ******************************************************************GBIFPGM 
05231  4196-BLD-EAC-EMC-BC-PMT-LVL.                                     GBIFPGM 
05232                                                                   GBIFPGM 
05233      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAMI'                    GBIFPGM 
05234         IF WS-PROCESS-CON                                         GBIFPGM 
05235            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05236                               TO WS-VALUE-LIMIT-S                 GBIFPGM 
05237            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05238              TO WS-PERCENT-VAL                                    GBIFPGM 
05239            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05240                               TO WS-PERCENT-VAL                   GBIFPGM 
05241            MOVE WS-PERCENT                                        GBIFPGM 
05242                               TO GCBH-EAC-EMC-BC-PMT-LVL-IN       GBIFPGM 
05243         END-IF                                                    GBIFPGM 
05244      END-IF                                                       GBIFPGM 
05245      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAMO'                    GBIFPGM 
05246         IF WS-PROCESS-GRP                                         GBIFPGM 
05247            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05248                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05249            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05250                                       TO WS-PERCENT-VAL           GBIFPGM 
05251            MOVE WS-PERCENT                                        GBIFPGM 
05252                               TO GCBH-EAC-EMC-BC-PMT-LVL-OUT      GBIFPGM 
05253         END-IF                                                    GBIFPGM 
05254      END-IF                                                       GBIFPGM 
05255      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EAMA'                    GBIFPGM 
05256         IF WS-PROCESS-CON                                         GBIFPGM 
05257            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05258                               TO WS-VALUE-LIMIT-S                 GBIFPGM 
05259            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05260                               TO WS-PERCENT-VAL                   GBIFPGM 
05261            MOVE WS-PERCENT                                        GBIFPGM 
05262                               TO GCBH-EAC-EMC-BC-PMT-LVL-OTH      GBIFPGM 
05263         END-IF                                                    GBIFPGM 
05264      END-IF.                                                      GBIFPGM 
05265                                                                   GBIFPGM 
05266  4196-EXIT.                                                       GBIFPGM 
05267      EXIT.                                                        GBIFPGM 
05268 /                                                                 GBIFPGM 
05269 *MQ 10/03                                                         GBIFPGM 
05270 ******************************************************************GBIFPGM 
05271 *                                                                 GBIFPGM 
05272 *    EAC/EMC BLUE SHIELD PAYMENT LEVEL                            GBIFPGM 
05273 *                                                                 GBIFPGM 
05274 ******************************************************************GBIFPGM 
05275  4197-BLD-EAC-EMC-BS-PMT-LVL.                                     GBIFPGM 
05276                                                                   GBIFPGM 
05277      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMAI'                    GBIFPGM 
05278         IF WS-PROCESS-CON                                         GBIFPGM 
05279            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05280                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05281            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05282              TO WS-PERCENT-VAL                                    GBIFPGM 
05283            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05284                                       TO WS-PERCENT-VAL           GBIFPGM 
05285            MOVE WS-PERCENT                                        GBIFPGM 
05286                                TO GCBH-EAC-EMC-BS-PMT-LVL-IN      GBIFPGM 
05287         END-IF                                                    GBIFPGM 
05288      END-IF                                                       GBIFPGM 
05289      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMAO'                    GBIFPGM 
05290         IF WS-PROCESS-GRP                                         GBIFPGM 
05291            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05292                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05293            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05294                                       TO WS-PERCENT-VAL           GBIFPGM 
05295            MOVE WS-PERCENT                                        GBIFPGM 
05296                                TO GCBH-EAC-EMC-BS-PMT-LVL-OUT     GBIFPGM 
05297         END-IF                                                    GBIFPGM 
05298      END-IF                                                       GBIFPGM 
05299      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMAA'                    GBIFPGM 
05300         IF WS-PROCESS-CON                                         GBIFPGM 
05301            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05302                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05303            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05304                                          TO WS-PERCENT-VAL        GBIFPGM 
05305            MOVE WS-PERCENT                                        GBIFPGM 
05306                                TO GCBH-EAC-EMC-BS-PMT-LVL-OTH     GBIFPGM 
05307         END-IF                                                    GBIFPGM 
05308      END-IF.                                                      GBIFPGM 
05309                                                                   GBIFPGM 
05310  4197-EXIT.                                                       GBIFPGM 
05311      EXIT.                                                        GBIFPGM 
05312 /                                                                 GBIFPGM 
05313 *MQ 10/03                                                         GBIFPGM 
05314 ******************************************************************GBIFPGM 
05315 *                                                                 GBIFPGM 
05316 *    EAC/EMC B/C AND B/S PAYMENT LEVEL                            GBIFPGM 
05317 *                                                                 GBIFPGM 
05318 ******************************************************************GBIFPGM 
05319  4198-BLD-EAC-EMC-BCBS-PMT-LVL.                                   GBIFPGM 
05320                                                                   GBIFPGM 
05321      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMEI'                    GBIFPGM 
05322         IF WS-PROCESS-CON                                         GBIFPGM 
05323            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05324                             TO WS-VALUE-LIMIT-S                   GBIFPGM 
05325            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05326              TO WS-PERCENT-VAL                                    GBIFPGM 
05327            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05328                             TO WS-PERCENT-VAL                     GBIFPGM 
05329            MOVE WS-PERCENT                                        GBIFPGM 
05330                             TO GCBH-EAC-EMC-BCBS-PMT-LVL-IN       GBIFPGM 
05331         END-IF                                                    GBIFPGM 
05332      END-IF                                                       GBIFPGM 
05333      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMEO'                    GBIFPGM 
05334         IF WS-PROCESS-GRP                                         GBIFPGM 
05335            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05336                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05337            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05338                             TO WS-PERCENT-VAL                     GBIFPGM 
05339            MOVE WS-PERCENT                                        GBIFPGM 
05340                             TO GCBH-EAC-EMC-BCBS-PMT-LVL-OUT      GBIFPGM 
05341         END-IF                                                    GBIFPGM 
05342      END-IF                                                       GBIFPGM 
05343      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'EMEA'                    GBIFPGM 
05344         IF WS-PROCESS-CON                                         GBIFPGM 
05345            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05346                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05347            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05348                                          TO WS-PERCENT-VAL        GBIFPGM 
05349            MOVE WS-PERCENT                                        GBIFPGM 
05350                             TO GCBH-EAC-EMC-BCBS-PMT-LVL-OTH      GBIFPGM 
05351         END-IF                                                    GBIFPGM 
05352      END-IF.                                                      GBIFPGM 
05353                                                                   GBIFPGM 
05354  4198-EXIT.                                                       GBIFPGM 
05355      EXIT.                                                        GBIFPGM 
05356 /                                                                 GBIFPGM 
05357 ******************************************************************GBIFPGM 
05358 *                                                                 GBIFPGM 
05359 *    SUPPLEMENTAL ACCIDENT CARE (90 DAYS MAXIMUM)                 GBIFPGM 
05360 *                                                                 GBIFPGM 
05361 ******************************************************************GBIFPGM 
05362  4200-BLD-SUPP-ACCID.                                             GBIFPGM 
05363                                                                   GBIFPGM 
05364      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SACI'                    GBIFPGM 
05365         IF WS-PROCESS-CON                                         GBIFPGM 
05366            IF GAB-COINS-VALUE-QUALIFIER (GAB-INDEX) = '5'         GBIFPGM 
05367               MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)              GBIFPGM 
05368                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05369               MOVE WS-VALUE-LIMIT-S      TO GCBH-SUPP-ACC-CARE-IN GBIFPGM 
05370            ELSE                                                   GBIFPGM 
05371               MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)              GBIFPGM 
05372                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05373               MOVE WS-VALUE-LIMIT        TO GCBH-SUPP-ACC-CARE-IN GBIFPGM 
05374            END-IF                                                 GBIFPGM 
05375         END-IF                                                    GBIFPGM 
05376      END-IF                                                       GBIFPGM 
05377      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SACO'                    GBIFPGM 
05378         IF WS-PROCESS-GRP                                         GBIFPGM 
05379            IF GAB-COINS-VALUE-QUALIFIER (GAB-INDEX) = '5'         GBIFPGM 
05380               MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)              GBIFPGM 
05381                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05382               MOVE WS-VALUE-LIMIT-S      TO GCBH-SUPP-ACC-CARE-OUTGBIFPGM 
05383            ELSE                                                   GBIFPGM 
05384               MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)              GBIFPGM 
05385                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05386               MOVE WS-VALUE-LIMIT        TO GCBH-SUPP-ACC-CARE-OUTGBIFPGM 
05387            END-IF                                                 GBIFPGM 
05388         END-IF                                                    GBIFPGM 
05389      END-IF                                                       GBIFPGM 
05390      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SACA'                    GBIFPGM 
05391         IF GAB-COINS-VALUE-QUALIFIER (GAB-INDEX) = '5'            GBIFPGM 
05392            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05393                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
05394            MOVE WS-VALUE-LIMIT-S      TO GCBH-SUPP-ACC-CARE-OTH   GBIFPGM 
05395         ELSE                                                      GBIFPGM 
05396            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05397                                       TO  WS-VALUE-LIMIT          GBIFPGM 
05398            MOVE WS-VALUE-LIMIT        TO GCBH-SUPP-ACC-CARE-OTH   GBIFPGM 
05399         END-IF                                                    GBIFPGM 
05400      END-IF.                                                      GBIFPGM 
05401                                                                   GBIFPGM 
05402  4200-EXIT.                                                       GBIFPGM 
05403      EXIT.                                                        GBIFPGM 
05404 /                                                                 GBIFPGM 
05405 ******************************************************************GBIFPGM 
05406 *                                                                 GBIFPGM 
05407 *    MEDICAL SURGICAL PAYMENT LEVEL                               GBIFPGM 
05408 *                                                                 GBIFPGM 
05409 ******************************************************************GBIFPGM 
05410  4210-BLD-MS-PMT-LVL.                                             GBIFPGM 
05411                                                                   GBIFPGM 
05412      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'MSPI'                    GBIFPGM 
05413         IF WS-PROCESS-CON                                         GBIFPGM 
05414            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05415                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05416            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05417              TO WS-PERCENT-VAL                                    GBIFPGM 
05418            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05419                                       TO WS-PERCENT-VAL           GBIFPGM 
05420            MOVE WS-PERCENT            TO GCBH-MED-SURG-PMT-LVL-IN GBIFPGM 
05421         END-IF                                                    GBIFPGM 
05422      END-IF                                                       GBIFPGM 
05423      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'MSPO'                    GBIFPGM 
05424         IF WS-PROCESS-GRP                                         GBIFPGM 
05425            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05426                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05427            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05428                                       TO WS-PERCENT-VAL           GBIFPGM 
05429            MOVE WS-PERCENT            TO GCBH-MED-SURG-PMT-LVL-OUTGBIFPGM 
05430         END-IF                                                    GBIFPGM 
05431      END-IF                                                       GBIFPGM 
05432      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'MSPA'                    GBIFPGM 
05433         IF WS-PROCESS-CON                                         GBIFPGM 
05434            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05435                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05436            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05437                                       TO WS-PERCENT-VAL           GBIFPGM 
05438            MOVE WS-PERCENT          TO GCBH-MED-SURG-PMT-LVL-OTH  GBIFPGM 
05439         END-IF                                                    GBIFPGM 
05440      END-IF.                                                      GBIFPGM 
05441                                                                   GBIFPGM 
05442  4210-EXIT.                                                       GBIFPGM 
05443      EXIT.                                                        GBIFPGM 
05444 /                                                                 GBIFPGM 
05445 *MQ 12/03/03                                                      GBIFPGM 
05446 ******************************************************************GBIFPGM 
05447 *                                                                 GBIFPGM 
05448 *    HOSPITAL / MEDICAL SURGICAL PAYMENT LEVEL                    GBIFPGM 
05449 *                                                                 GBIFPGM 
05450 ******************************************************************GBIFPGM 
05451  4215-BLD-HOSP-MS-PMT-LVL.                                        GBIFPGM 
05452                                                                   GBIFPGM 
05453      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'HMSI'                    GBIFPGM 
05454         IF WS-PROCESS-CON                                         GBIFPGM 
05455            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05456                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05457            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05458              TO WS-PERCENT-VAL                                    GBIFPGM 
05459            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05460                                 TO WS-PERCENT-VAL                 GBIFPGM 
05461            MOVE WS-PERCENT                                        GBIFPGM 
05462                                 TO GCBH-HOSP-MEDSURG-PMT-LVL-IN   GBIFPGM 
05463         END-IF                                                    GBIFPGM 
05464      END-IF                                                       GBIFPGM 
05465      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'HMSO'                    GBIFPGM 
05466         IF WS-PROCESS-GRP                                         GBIFPGM 
05467            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05468                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05469            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05470                                 TO WS-PERCENT-VAL                 GBIFPGM 
05471            MOVE WS-PERCENT                                        GBIFPGM 
05472                                 TO GCBH-HOSP-MEDSURG-PMT-LVL-OUT  GBIFPGM 
05473         END-IF                                                    GBIFPGM 
05474      END-IF                                                       GBIFPGM 
05475      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'HMSA'                    GBIFPGM 
05476         IF WS-PROCESS-CON                                         GBIFPGM 
05477            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
05478                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
05479            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05480                                 TO WS-PERCENT-VAL                 GBIFPGM 
05481            MOVE WS-PERCENT                                        GBIFPGM 
05482                                 TO GCBH-HOSP-MEDSURG-PMT-LVL-OTH  GBIFPGM 
05483         END-IF                                                    GBIFPGM 
05484      END-IF.                                                      GBIFPGM 
05485                                                                   GBIFPGM 
05486  4215-EXIT.                                                       GBIFPGM 
05487      EXIT.                                                        GBIFPGM 
05488 /                                                                 GBIFPGM 
05489 ******************************************************************GBIFPGM 
05490 *                                                                 GBIFPGM 
05491 *    THERAPY MAXIMUM COMBINED (PT, OT, ST)                        GBIFPGM 
05492 *                                                                 GBIFPGM 
05493 ******************************************************************GBIFPGM 
05494  4220-BLD-THERAPY-MAX-COMB.                                       GBIFPGM 
05495                                                                   GBIFPGM 
05496 *MQ 6/05/03                                                       GBIFPGM 
05497      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'THMI'                     GBIFPGM 
05498         IF WS-PROCESS-CON                                         GBIFPGM 
05499            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05500            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-THER-MAX-COMB-IN      GBIFPGM 
05501         END-IF                                                    GBIFPGM 
05502      END-IF                                                       GBIFPGM 
05503      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'THMO'                     GBIFPGM 
05504         IF WS-PROCESS-GRP                                         GBIFPGM 
05505            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05506            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-THER-MAX-COMB-OUT     GBIFPGM 
05507         END-IF                                                    GBIFPGM 
05508      END-IF                                                       GBIFPGM 
05509      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'THMA'                     GBIFPGM 
05510         IF WS-PROCESS-CON                                         GBIFPGM 
05511            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05512            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-THER-MAX-COMB-OTH     GBIFPGM 
05513         END-IF                                                    GBIFPGM 
05514      END-IF.                                                      GBIFPGM 
05515                                                                   GBIFPGM 
05516  4220-EXIT.                                                       GBIFPGM 
05517      EXIT.                                                        GBIFPGM 
05518 /                                                                 GBIFPGM 
05519 ******************************************************************GBIFPGM 
05520 *                                                                 GBIFPGM 
05521 *    FUNCTIONAL OCCUPATIONAL THERAPY MAXIMUM                      GBIFPGM 
05522 *                                                                 GBIFPGM 
05523 ******************************************************************GBIFPGM 
05524  4230-BLD-FUNC-OCC-THER-MAX.                                      GBIFPGM 
05525                                                                   GBIFPGM 
05526      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'OTMI'                     GBIFPGM 
05527         IF WS-PROCESS-CON                                         GBIFPGM 
05528            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05529            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-FUNC-OC-THER-MAX-IN   GBIFPGM 
05530         END-IF                                                    GBIFPGM 
05531      END-IF                                                       GBIFPGM 
05532      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'OTMO'                     GBIFPGM 
05533         IF WS-PROCESS-GRP                                         GBIFPGM 
05534            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05535            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-FUNC-OC-THER-MAX-OUT  GBIFPGM 
05536         END-IF                                                    GBIFPGM 
05537      END-IF                                                       GBIFPGM 
05538      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'OTMA'                     GBIFPGM 
05539         IF WS-PROCESS-CON                                         GBIFPGM 
05540            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05541            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-FUNC-OC-THER-MAX-OTH  GBIFPGM 
05542         END-IF                                                    GBIFPGM 
05543      END-IF.                                                      GBIFPGM 
05544                                                                   GBIFPGM 
05545  4230-EXIT.                                                       GBIFPGM 
05546      EXIT.                                                        GBIFPGM 
05547 /                                                                 GBIFPGM 
05548 ******************************************************************GBIFPGM 
05549 *                                                                 GBIFPGM 
05550 *    PHYSICAL / MECHANO THERAPY MAXIMUM                           GBIFPGM 
05551 *                                                                 GBIFPGM 
05552 ******************************************************************GBIFPGM 
05553  4240-BLD-PHYS-MECH-THER-MAX.                                     GBIFPGM 
05554                                                                   GBIFPGM 
05555      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'PTMI'                     GBIFPGM 
05556         IF WS-PROCESS-CON                                         GBIFPGM 
05557            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05558            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-PHYS-ME-THER-MAX-IN   GBIFPGM 
05559         END-IF                                                    GBIFPGM 
05560      END-IF                                                       GBIFPGM 
05561      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'PTMO'                     GBIFPGM 
05562         IF WS-PROCESS-GRP                                         GBIFPGM 
05563            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05564            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-PHYS-ME-THER-MAX-OUT  GBIFPGM 
05565         END-IF                                                    GBIFPGM 
05566      END-IF                                                       GBIFPGM 
05567      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'PTMA'                     GBIFPGM 
05568         IF WS-PROCESS-CON                                         GBIFPGM 
05569            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05570            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-PHYS-ME-THER-MAX-OTH  GBIFPGM 
05571         END-IF                                                    GBIFPGM 
05572      END-IF.                                                      GBIFPGM 
05573                                                                   GBIFPGM 
05574                                                                   GBIFPGM 
05575  4240-EXIT.                                                       GBIFPGM 
05576      EXIT.                                                        GBIFPGM 
05577 /                                                                 GBIFPGM 
05578 ******************************************************************GBIFPGM 
05579 *                                                                 GBIFPGM 
05580 *    SPEECH THERAPY MAXIMUM                                       GBIFPGM 
05581 *                                                                 GBIFPGM 
05582 ******************************************************************GBIFPGM 
05583  4250-BLD-SPEECH-THER-MAX.                                        GBIFPGM 
05584                                                                   GBIFPGM 
05585 *MQ 6/05/03                                                       GBIFPGM 
05586      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'STMI'                     GBIFPGM 
05587         IF WS-PROCESS-CON                                         GBIFPGM 
05588            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05589            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SPEECH-THER-MAX-IN    GBIFPGM 
05590         END-IF                                                    GBIFPGM 
05591      END-IF                                                       GBIFPGM 
05592      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'STMO'                     GBIFPGM 
05593         IF WS-PROCESS-GRP                                         GBIFPGM 
05594            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05595            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SPEECH-THER-MAX-OUT   GBIFPGM 
05596         END-IF                                                    GBIFPGM 
05597      END-IF                                                       GBIFPGM 
05598      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'STMA'                     GBIFPGM 
05599         IF WS-PROCESS-CON                                         GBIFPGM 
05600            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05601            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SPEECH-THER-MAX-OTH   GBIFPGM 
05602         END-IF                                                    GBIFPGM 
05603      END-IF.                                                      GBIFPGM 
05604                                                                   GBIFPGM 
05605  4250-EXIT.                                                       GBIFPGM 
05606      EXIT.                                                        GBIFPGM 
05607 /                                                                 GBIFPGM 
05608 ******************************************************************GBIFPGM 
05609 *                                                                 GBIFPGM 
05610 *    TMJ LIFETIME MAXIMUM                                         GBIFPGM 
05611 *                                                                 GBIFPGM 
05612 ******************************************************************GBIFPGM 
05613  4260-BLD-TMJ-LIFETIME-MAX.                                       GBIFPGM 
05614                                                                   GBIFPGM 
05615      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'TMJI'                     GBIFPGM 
05616         IF WS-PROCESS-CON                                         GBIFPGM 
05617            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05618               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05619                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05620               MOVE WS-VALUE-LIMIT-S      TO GCBH-TMJ-LIFE-MAX-IN  GBIFPGM 
05621            ELSE                                                   GBIFPGM 
05622               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05623                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05624               MOVE WS-VALUE-LIMIT        TO GCBH-TMJ-LIFE-MAX-IN  GBIFPGM 
05625            END-IF                                                 GBIFPGM 
05626         END-IF                                                    GBIFPGM 
05627      END-IF                                                       GBIFPGM 
05628      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'TMJO'                     GBIFPGM 
05629         IF WS-PROCESS-GRP                                         GBIFPGM 
05630            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05631               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05632                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05633               MOVE WS-VALUE-LIMIT-S      TO GCBH-TMJ-LIFE-MAX-OUT GBIFPGM 
05634            ELSE                                                   GBIFPGM 
05635               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05636                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05637               MOVE WS-VALUE-LIMIT        TO GCBH-TMJ-LIFE-MAX-OUT GBIFPGM 
05638            END-IF                                                 GBIFPGM 
05639         END-IF                                                    GBIFPGM 
05640      END-IF                                                       GBIFPGM 
05641      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'TMJA'                     GBIFPGM 
05642         IF WS-PROCESS-CON                                         GBIFPGM 
05643            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05644               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05645                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05646               MOVE WS-VALUE-LIMIT-S      TO GCBH-TMJ-LIFE-MAX-OTH GBIFPGM 
05647            ELSE                                                   GBIFPGM 
05648               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05649                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05650               MOVE WS-VALUE-LIMIT        TO GCBH-TMJ-LIFE-MAX-OTH GBIFPGM 
05651            END-IF                                                 GBIFPGM 
05652         END-IF                                                    GBIFPGM 
05653      END-IF.                                                      GBIFPGM 
05654                                                                   GBIFPGM 
05655  4260-EXIT.                                                       GBIFPGM 
05656      EXIT.                                                        GBIFPGM 
05657 /                                                                 GBIFPGM 
05658 ******************************************************************GBIFPGM 
05659 *                                                                 GBIFPGM 
05660 *    PRIVATE DUTY NURSING MAXIMUM                                 GBIFPGM 
05661 *                                                                 GBIFPGM 
05662 ******************************************************************GBIFPGM 
05663  4270-BLD-PRIV-DUTY-NURSE-MAX.                                    GBIFPGM 
05664                                                                   GBIFPGM 
05665 *MQ 6/05/03                                                       GBIFPGM 
05666      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'PDNA'                     GBIFPGM 
05667         IF WS-PROCESS-CON                                         GBIFPGM 
05668            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05669            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-PRI-DUTY-NUR-MAX-OTH  GBIFPGM 
05670         END-IF                                                    GBIFPGM 
05671      END-IF                                                       GBIFPGM 
05672      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'PDNA'                     GBIFPGM 
05673         IF WS-PROCESS-GRP                                         GBIFPGM 
05674            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05675            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-PRI-DUTY-NUR-MAX-OTH  GBIFPGM 
05676         END-IF                                                    GBIFPGM 
05677      END-IF.                                                      GBIFPGM 
05678                                                                   GBIFPGM 
05679  4270-EXIT.                                                       GBIFPGM 
05680      EXIT.                                                        GBIFPGM 
05681 /                                                                 GBIFPGM 
05682 *MQ 10/03                                                         GBIFPGM 
05683 ******************************************************************GBIFPGM 
05684 *                                                                 GBIFPGM 
05685 *    SKILLED NURSING BENEFIT PERIOD MAXIMUM                       GBIFPGM 
05686 *                                                                 GBIFPGM 
05687 ******************************************************************GBIFPGM 
05688  4275-BLD-SKILL-NUR-BP-MAX.                                       GBIFPGM 
05689                                                                   GBIFPGM 
05690      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SNCI'                     GBIFPGM 
05691         IF WS-PROCESS-CON                                         GBIFPGM 
05692            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05693            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SKILL-NUR-BP-MAX-IN   GBIFPGM 
05694         END-IF                                                    GBIFPGM 
05695      END-IF                                                       GBIFPGM 
05696      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SNCO'                     GBIFPGM 
05697         IF WS-PROCESS-GRP                                         GBIFPGM 
05698            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05699            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SKILL-NUR-BP-MAX-OUT  GBIFPGM 
05700         END-IF                                                    GBIFPGM 
05701      END-IF                                                       GBIFPGM 
05702      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SNCA'                     GBIFPGM 
05703         IF WS-PROCESS-CON                                         GBIFPGM 
05704            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05705            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SKILL-NUR-BP-MAX-OTH  GBIFPGM 
05706         END-IF                                                    GBIFPGM 
05707      END-IF.                                                      GBIFPGM 
05708                                                                   GBIFPGM 
05709  4275-EXIT.                                                       GBIFPGM 
05710      EXIT.                                                        GBIFPGM 
05711 /                                                                 GBIFPGM 
05712 ******************************************************************GBIFPGM 
05713 *                                                                 GBIFPGM 
05714 *    CHIROPRACTIC SERVICES MAXIMUM                                GBIFPGM 
05715 *                                                                 GBIFPGM 
05716 ******************************************************************GBIFPGM 
05717  4280-BLD-CHIRO-SERV-MAX.                                         GBIFPGM 
05718                                                                   GBIFPGM 
05719 *MQ 6/05/03                                                       GBIFPGM 
05720      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CHSI'                     GBIFPGM 
05721         IF WS-PROCESS-CON                                         GBIFPGM 
05722            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05723            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-CHIRO-SERV-MAX-IN     GBIFPGM 
05724         END-IF                                                    GBIFPGM 
05725      END-IF                                                       GBIFPGM 
05726      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CHSO'                     GBIFPGM 
05727         IF WS-PROCESS-GRP                                         GBIFPGM 
05728            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05729            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-CHIRO-SERV-MAX-OUT    GBIFPGM 
05730         END-IF                                                    GBIFPGM 
05731      END-IF                                                       GBIFPGM 
05732      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CHSA'                     GBIFPGM 
05733         IF WS-PROCESS-CON                                         GBIFPGM 
05734            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05735            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-CHIRO-SERV-MAX-OTH    GBIFPGM 
05736         END-IF                                                    GBIFPGM 
05737      END-IF.                                                      GBIFPGM 
05738                                                                   GBIFPGM 
05739  4280-EXIT.                                                       GBIFPGM 
05740      EXIT.                                                        GBIFPGM 
05741 /                                                                 GBIFPGM 
05742 ******************************************************************GBIFPGM 
05743 *                                                                 GBIFPGM 
05744 *    CHIROPRACTOR PROVIDER MAXIMUM                                GBIFPGM 
05745 *                                                                 GBIFPGM 
05746 ******************************************************************GBIFPGM 
05747  4290-BLD-CHIRO-PROV-MAX.                                         GBIFPGM 
05748                                                                   GBIFPGM 
05749 *MQ 05/04                                                         GBIFPGM 
05750      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CPMI'                     GBIFPGM 
05751         IF WS-PROCESS-CON                                         GBIFPGM 
05752            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05753            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-CHIRO-PROV-MAX-IN     GBIFPGM 
05754         END-IF                                                    GBIFPGM 
05755      END-IF                                                       GBIFPGM 
05756      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CPMO'                     GBIFPGM 
05757         IF WS-PROCESS-GRP                                         GBIFPGM 
05758            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05759            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-CHIRO-PROV-MAX-OUT    GBIFPGM 
05760         END-IF                                                    GBIFPGM 
05761      END-IF                                                       GBIFPGM 
05762      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CPMA'                     GBIFPGM 
05763         IF WS-PROCESS-CON                                         GBIFPGM 
05764            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
05765            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-CHIRO-PROV-MAX-OTH    GBIFPGM 
05766         END-IF                                                    GBIFPGM 
05767      END-IF.                                                      GBIFPGM 
05768                                                                   GBIFPGM 
05769  4290-EXIT.                                                       GBIFPGM 
05770      EXIT.                                                        GBIFPGM 
05771 /                                                                 GBIFPGM 
05772 ******************************************************************GBIFPGM 
05773 *                                                                 GBIFPGM 
05774 *    WELL CARE PAYMENT LEVEL                                      GBIFPGM 
05775 *                                                                 GBIFPGM 
05776 ******************************************************************GBIFPGM 
05777  4300-BLD-WELL-CARE-PMT-LVL.                                      GBIFPGM 
05778                                                                   GBIFPGM 
05779      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WELI'                    GBIFPGM 
05780         IF WS-PROCESS-CON                                         GBIFPGM 
05781            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05782                                       TO WS-PERCENT-VAL           GBIFPGM 
05783            MOVE WS-PERCENT         TO GCBH-WELL-CARE-PMT-LVL-IN   GBIFPGM 
05784         END-IF                                                    GBIFPGM 
05785      END-IF                                                       GBIFPGM 
05786      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WELO'                    GBIFPGM 
05787         IF WS-PROCESS-GRP                                         GBIFPGM 
05788            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05789                                       TO WS-PERCENT-VAL           GBIFPGM 
05790            MOVE WS-PERCENT         TO GCBH-WELL-CARE-PMT-LVL-OUT  GBIFPGM 
05791         END-IF                                                    GBIFPGM 
05792      END-IF                                                       GBIFPGM 
05793      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WELA'                    GBIFPGM 
05794         IF WS-PROCESS-CON                                         GBIFPGM 
05795            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05796                                          TO WS-PERCENT-VAL        GBIFPGM 
05797            MOVE WS-PERCENT         TO GCBH-WELL-CARE-PMT-LVL-OTH  GBIFPGM 
05798         END-IF                                                    GBIFPGM 
05799      END-IF.                                                      GBIFPGM 
05800                                                                   GBIFPGM 
05801  4300-EXIT.                                                       GBIFPGM 
05802      EXIT.                                                        GBIFPGM 
05803 /                                                                 GBIFPGM 
05804 *MQ 10/03                                                         GBIFPGM 
05805 ******************************************************************GBIFPGM 
05806 *                                                                 GBIFPGM 
05807 *    WELL ADULT CARE PAYMENT LEVEL                                GBIFPGM 
05808 *                                                                 GBIFPGM 
05809 ******************************************************************GBIFPGM 
05810  4305-BLD-WELL-AD-CARE-PMT-LVL.                                   GBIFPGM 
05811                                                                   GBIFPGM 
05812      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WAPI'                    GBIFPGM 
05813         IF WS-PROCESS-CON                                         GBIFPGM 
05814            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05815                                TO WS-PERCENT-VAL                  GBIFPGM 
05816            MOVE WS-PERCENT     TO GCBH-WELL-AD-CARE-PMT-LVL-IN    GBIFPGM 
05817         END-IF                                                    GBIFPGM 
05818      END-IF                                                       GBIFPGM 
05819      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WAPO'                    GBIFPGM 
05820         IF WS-PROCESS-GRP                                         GBIFPGM 
05821            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05822                                       TO WS-PERCENT-VAL           GBIFPGM 
05823            MOVE WS-PERCENT     TO GCBH-WELL-AD-CARE-PMT-LVL-OUT   GBIFPGM 
05824         END-IF                                                    GBIFPGM 
05825      END-IF                                                       GBIFPGM 
05826      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WAPA'                    GBIFPGM 
05827         IF WS-PROCESS-CON                                         GBIFPGM 
05828            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
05829                                TO WS-PERCENT-VAL                  GBIFPGM 
05830            MOVE WS-PERCENT     TO GCBH-WELL-AD-CARE-PMT-LVL-OTH   GBIFPGM 
05831         END-IF                                                    GBIFPGM 
05832      END-IF.                                                      GBIFPGM 
05833                                                                   GBIFPGM 
05834  4305-EXIT.                                                       GBIFPGM 
05835      EXIT.                                                        GBIFPGM 
05836 /                                                                 GBIFPGM 
05837 ******************************************************************GBIFPGM 
05838 *                                                                 GBIFPGM 
05839 *    WELL ADULT CARE MAXIMUM                                      GBIFPGM 
05840 *                                                                 GBIFPGM 
05841 ******************************************************************GBIFPGM 
05842  4310-BLD-WELL-ADLT-CARE-MAX.                                     GBIFPGM 
05843                                                                   GBIFPGM 
05844      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WACI'                     GBIFPGM 
05845         IF WS-PROCESS-CON                                         GBIFPGM 
05846            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05847               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05848                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05849               MOVE WS-VALUE-LIMIT-S TO GCBH-WELL-AD-CARE-MAX-IN   GBIFPGM 
05850            ELSE                                                   GBIFPGM 
05851               IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '2'       GBIFPGM 
05852                  MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)            GBIFPGM 
05853                                          TO  WS-VALUE-LIMIT-V     GBIFPGM 
05854                  MOVE WS-VALUE-LIMIT-FULL   TO  WS-VISITS-VAL     GBIFPGM 
05855                  MOVE WS-VISITS  TO GCBH-WELL-AD-CARE-MAX-IN      GBIFPGM 
05856               ELSE                                                GBIFPGM 
05857                  MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)            GBIFPGM 
05858                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05859                  MOVE WS-VALUE-LIMIT TO GCBH-WELL-AD-CARE-MAX-IN  GBIFPGM 
05860               END-IF                                              GBIFPGM 
05861            END-IF                                                 GBIFPGM 
05862         END-IF                                                    GBIFPGM 
05863      END-IF                                                       GBIFPGM 
05864                                                                   GBIFPGM 
05865      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WACO'                     GBIFPGM 
05866         IF WS-PROCESS-GRP                                         GBIFPGM 
05867            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05868               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05869                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05870               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-AD-CARE-MAX-OUTGBIFPGM 
05871 *JP 3/21/03                                                       GBIFPGM 
05872               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05873                                          TO  WS-WRK-VAL-1         GBIFPGM 
05874               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 - 150.00        GBIFPGM 
05875               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
05876               MOVE WS-WRK-VAL-BUX TO GCBH-WELL-AD-CARE-MAX-OUT-UTLGBIFPGM 
05877            ELSE                                                   GBIFPGM 
05878               IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '2'       GBIFPGM 
05879                  MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)            GBIFPGM 
05880                                          TO  WS-VALUE-LIMIT-V     GBIFPGM 
05881                  MOVE WS-VALUE-LIMIT-FULL TO  WS-VISITS-VAL       GBIFPGM 
05882                  MOVE WS-VISITS TO GCBH-WELL-AD-CARE-MAX-OUT      GBIFPGM 
05883               ELSE                                                GBIFPGM 
05884                  MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)            GBIFPGM 
05885                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05886                  MOVE WS-VALUE-LIMIT TO GCBH-WELL-AD-CARE-MAX-OUT GBIFPGM 
05887               END-IF                                              GBIFPGM 
05888            END-IF                                                 GBIFPGM 
05889         END-IF                                                    GBIFPGM 
05890      END-IF.                                                      GBIFPGM 
05891                                                                   GBIFPGM 
05892      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WACA'                     GBIFPGM 
05893         IF WS-PROCESS-CON                                         GBIFPGM 
05894            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05895               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05896                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05897               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-AD-CARE-MAX-OTHGBIFPGM 
05898            ELSE                                                   GBIFPGM 
05899               IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '2'       GBIFPGM 
05900                  MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)            GBIFPGM 
05901                                          TO  WS-VALUE-LIMIT-V     GBIFPGM 
05902                  MOVE WS-VALUE-LIMIT-FULL TO WS-VISITS-VAL        GBIFPGM 
05903                  MOVE WS-VISITS TO GCBH-WELL-AD-CARE-MAX-OTH      GBIFPGM 
05904               ELSE                                                GBIFPGM 
05905                  MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)            GBIFPGM 
05906                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05907                  MOVE WS-VALUE-LIMIT TO GCBH-WELL-AD-CARE-MAX-OTH GBIFPGM 
05908               END-IF                                              GBIFPGM 
05909            END-IF                                                 GBIFPGM 
05910         END-IF                                                    GBIFPGM 
05911      END-IF.                                                      GBIFPGM 
05912                                                                   GBIFPGM 
05913  4310-EXIT.                                                       GBIFPGM 
05914      EXIT.                                                        GBIFPGM 
05915 /                                                                 GBIFPGM 
05916 *MQ 10/03                                                         GBIFPGM 
05917 ******************************************************************GBIFPGM 
05918 *                                                                 GBIFPGM 
05919 *    WELL ADULT CARE BENEFIT PERIOD MAXIMUM                       GBIFPGM 
05920 *                                                                 GBIFPGM 
05921 ******************************************************************GBIFPGM 
05922  4315-BLD-WELL-AD-CARE-BP-MAX.                                    GBIFPGM 
05923                                                                   GBIFPGM 
05924      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WCMI'                     GBIFPGM 
05925         IF WS-PROCESS-CON                                         GBIFPGM 
05926            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05927               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05928                             TO  WS-VALUE-LIMIT-S                  GBIFPGM 
05929               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
05930                             TO GCBH-WELL-AD-CARE-BP-MAX-IN        GBIFPGM 
05931            ELSE                                                   GBIFPGM 
05932               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05933                             TO  WS-VALUE-LIMIT                    GBIFPGM 
05934               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
05935                             TO GCBH-WELL-AD-CARE-BP-MAX-IN        GBIFPGM 
05936            END-IF                                                 GBIFPGM 
05937         END-IF                                                    GBIFPGM 
05938      END-IF                                                       GBIFPGM 
05939      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WCMO'                     GBIFPGM 
05940         IF WS-PROCESS-GRP                                         GBIFPGM 
05941            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05942               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05943                             TO  WS-VALUE-LIMIT-S                  GBIFPGM 
05944               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
05945                             TO GCBH-WELL-AD-CARE-BP-MAX-OUT       GBIFPGM 
05946            ELSE                                                   GBIFPGM 
05947               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05948                             TO  WS-VALUE-LIMIT                    GBIFPGM 
05949               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
05950                             TO GCBH-WELL-AD-CARE-BP-MAX-OUT       GBIFPGM 
05951            END-IF                                                 GBIFPGM 
05952         END-IF.                                                   GBIFPGM 
05953                                                                   GBIFPGM 
05954      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WCMA'                     GBIFPGM 
05955         IF WS-PROCESS-CON                                         GBIFPGM 
05956            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05957               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05958                             TO  WS-VALUE-LIMIT-S                  GBIFPGM 
05959               MOVE WS-VALUE-LIMIT-S                               GBIFPGM 
05960                             TO GCBH-WELL-AD-CARE-BP-MAX-OTH       GBIFPGM 
05961            ELSE                                                   GBIFPGM 
05962               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05963                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05964               MOVE WS-VALUE-LIMIT                                 GBIFPGM 
05965                             TO GCBH-WELL-AD-CARE-BP-MAX-OTH       GBIFPGM 
05966            END-IF.                                                GBIFPGM 
05967                                                                   GBIFPGM 
05968  4315-EXIT.                                                       GBIFPGM 
05969      EXIT.                                                        GBIFPGM 
05970 /                                                                 GBIFPGM 
05971 ******************************************************************GBIFPGM 
05972 *                                                                 GBIFPGM 
05973 *    WELL CHILE CARE MAXIMUM                                      GBIFPGM 
05974 *                                                                 GBIFPGM 
05975 ******************************************************************GBIFPGM 
05976  4320-BLD-WELL-CHILD-CARE-MAX.                                    GBIFPGM 
05977                                                                   GBIFPGM 
05978      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WBCI'                     GBIFPGM 
05979         IF WS-PROCESS-CON                                         GBIFPGM 
05980            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05981               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05982                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05983               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-CH-CARE-MAX-IN GBIFPGM 
05984            ELSE                                                   GBIFPGM 
05985               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05986                                          TO  WS-VALUE-LIMIT       GBIFPGM 
05987               MOVE WS-VALUE-LIMIT     TO GCBH-WELL-CH-CARE-MAX-IN GBIFPGM 
05988            END-IF                                                 GBIFPGM 
05989         END-IF                                                    GBIFPGM 
05990      ELSE                                                         GBIFPGM 
05991      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WBCO'                     GBIFPGM 
05992         IF WS-PROCESS-GRP                                         GBIFPGM 
05993            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
05994               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05995                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
05996               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-CH-CARE-MAX-OUTGBIFPGM 
05997 *JP 3/21/03                                                       GBIFPGM 
05998               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
05999                                          TO  WS-WRK-VAL-1         GBIFPGM 
06000               COMPUTE WS-WRK-VAL-2 = WS-WRK-VAL-1 - 380.00        GBIFPGM 
06001               MOVE WS-WRK-VAL-2 TO WS-WRK-VAL-BUX                 GBIFPGM 
06002               MOVE WS-WRK-VAL-BUX TO GCBH-WELL-CH-CARE-MAX-OUT-UTLGBIFPGM 
06003            ELSE                                                   GBIFPGM 
06004               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
06005                                          TO  WS-VALUE-LIMIT       GBIFPGM 
06006               MOVE WS-VALUE-LIMIT     TO GCBH-WELL-CH-CARE-MAX-OUTGBIFPGM 
06007            END-IF                                                 GBIFPGM 
06008         END-IF                                                    GBIFPGM 
06009      ELSE                                                         GBIFPGM 
06010      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'WBCA'                     GBIFPGM 
06011         IF WS-PROCESS-CON                                         GBIFPGM 
06012            IF GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX) = '5'          GBIFPGM 
06013               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
06014                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
06015               MOVE WS-VALUE-LIMIT-S   TO GCBH-WELL-CH-CARE-MAX-OTHGBIFPGM 
06016            ELSE                                                   GBIFPGM 
06017               MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)               GBIFPGM 
06018                                          TO  WS-VALUE-LIMIT       GBIFPGM 
06019               MOVE WS-VALUE-LIMIT     TO GCBH-WELL-CH-CARE-MAX-OTHGBIFPGM 
06020            END-IF.                                                GBIFPGM 
06021                                                                   GBIFPGM 
06022  4320-EXIT.                                                       GBIFPGM 
06023      EXIT.                                                        GBIFPGM 
06024 /                                                                 GBIFPGM 
06025 ******************************************************************GBIFPGM 
06026 *                                                                 GBIFPGM 
06027 *    WELL CHILD CARE PAYMENT LEVEL                                GBIFPGM 
06028 *                                                                 GBIFPGM 
06029 ******************************************************************GBIFPGM 
06030  4325-BLD-WELL-CH-CARE-PMT-LVL.                                   GBIFPGM 
06031                                                                   GBIFPGM 
06032      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WCPI'                    GBIFPGM 
06033         IF WS-PROCESS-CON                                         GBIFPGM 
06034            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06035                                  TO WS-PERCENT-VAL                GBIFPGM 
06036            MOVE WS-PERCENT       TO GCBH-WELL-CH-CARE-PMT-LVL-IN  GBIFPGM 
06037         END-IF                                                    GBIFPGM 
06038      END-IF                                                       GBIFPGM 
06039      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WCPO'                    GBIFPGM 
06040         IF WS-PROCESS-GRP                                         GBIFPGM 
06041            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06042                                  TO WS-PERCENT-VAL                GBIFPGM 
06043            MOVE WS-PERCENT       TO GCBH-WELL-CH-CARE-PMT-LVL-OUT GBIFPGM 
06044         END-IF                                                    GBIFPGM 
06045      END-IF                                                       GBIFPGM 
06046      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'WCPA'                    GBIFPGM 
06047         IF WS-PROCESS-CON                                         GBIFPGM 
06048            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06049                                  TO WS-PERCENT-VAL                GBIFPGM 
06050            MOVE WS-PERCENT       TO GCBH-WELL-CH-CARE-PMT-LVL-OTH GBIFPGM 
06051         END-IF                                                    GBIFPGM 
06052      END-IF.                                                      GBIFPGM 
06053                                                                   GBIFPGM 
06054  4325-EXIT.                                                       GBIFPGM 
06055      EXIT.                                                        GBIFPGM 
06056 /                                                                 GBIFPGM 
06057 ******************************************************************GBIFPGM 
06058 *                                                                 GBIFPGM 
06059 *    UNSOLICITED PROVIDERS PAYMENT LEVEL                          GBIFPGM 
06060 *    (FKA OTHER COVERED SERVICES PAYMENT LEVEL)                   GBIFPGM 
06061 *                                                                 GBIFPGM 
06062 ******************************************************************GBIFPGM 
06063  4330-BLD-OTHER-CS-PMT-LVL.                                       GBIFPGM 
06064                                                                   GBIFPGM 
06065 *MQ 6/05/03 DELETED LOGIC FOR 'OCSI' AND 'OSCO' ACCUM ID          GBIFPGM 
06066 *           PER SSD                                               GBIFPGM 
06067                                                                   GBIFPGM 
06068      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'OCSA'                    GBIFPGM 
06069 ***     IF WS-PROCESS-CON                                         GBIFPGM 
06070            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06071                                       TO WS-PERCENT-VAL           GBIFPGM 
06072            MOVE WS-PERCENT            TO GCBH-OTH-COV-PMT-LVL-OTH GBIFPGM 
06073 ***     END-IF                                                    GBIFPGM 
06074      END-IF.                                                      GBIFPGM 
06075                                                                   GBIFPGM 
06076  4330-EXIT.                                                       GBIFPGM 
06077      EXIT.                                                        GBIFPGM 
06078 /                                                                 GBIFPGM 
06079 ******************************************************************GBIFPGM 
06080 *                                                                 GBIFPGM 
06081 *    MSA SANCTION COINSURANCE                                     GBIFPGM 
06082 *                                                                 GBIFPGM 
06083 ******************************************************************GBIFPGM 
06084  4340-BLD-MSA-SANCTION-COINS.                                     GBIFPGM 
06085                                                                   GBIFPGM 
06086      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'MSAC'                    GBIFPGM 
06087         IF WS-PROCESS-GRP                                         GBIFPGM 
06088            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06089 * MQ 02/13/04                                                     GBIFPGM 
06090                              TO WS-COINS-PERCENT-LEVEL-1          GBIFPGM 
06091                                                                   GBIFPGM 
06092      COMPUTE WS-COINS-PERCENT-LEVEL-2 =                           GBIFPGM 
06093           100 - WS-COINS-PERCENT-LEVEL-1                          GBIFPGM 
06094                                                                   GBIFPGM 
06095      MOVE WS-COINS-PERCENT-LEVEL-2   TO WS-PERCENT-VAL            GBIFPGM 
06096            MOVE WS-PERCENT            TO GCBH-MSA-SANC-COINS-OTH. GBIFPGM 
06097                                                                   GBIFPGM 
06098  4340-EXIT.                                                       GBIFPGM 
06099      EXIT.                                                        GBIFPGM 
06100 /                                                                 GBIFPGM 
06101 ******************************************************************GBIFPGM 
06102 *                                                                 GBIFPGM 
06103 *    MSA SANCTION DEDUCTIBLE                                      GBIFPGM 
06104 *                                                                 GBIFPGM 
06105 ******************************************************************GBIFPGM 
06106  4350-BLD-MSA-SANCTION-DED.                                       GBIFPGM 
06107                                                                   GBIFPGM 
07685 *MQ 05/03/05                                                      GBIFPGM 
07686      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'MSID'                     GBIFPGM 
07687         IF WS-PROCESS-CON                                         GBIFPGM 
07688            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
07689               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
07690                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
07691               MOVE WS-VALUE-LIMIT-S   TO GCBH-MSA-SANC-DED-IN     GBIFPGM 
07692               MOVE 'Y'                TO WS-MSID-VALQUAL5-SW      GBIFPGM 
07693            ELSE                                                   GBIFPGM 
07694               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
07695                                       TO  WS-VALUE-LIMIT          GBIFPGM 
07696               MOVE WS-VALUE-LIMIT     TO GCBH-MSA-SANC-DED-IN     GBIFPGM 
07697            END-IF                                                 GBIFPGM 
07698         END-IF                                                    GBIFPGM 
07699      ELSE                                                         GBIFPGM 
07700      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'MSOD'                     GBIFPGM 
06109         IF WS-PROCESS-GRP                                         GBIFPGM 
06110            IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'          GBIFPGM 
06111               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
07704                                      TO  WS-VALUE-LIMIT-S         GBIFPGM 
07705               MOVE WS-VALUE-LIMIT-S  TO GCBH-MSA-SANC-DED-OUT     GBIFPGM 
07706               MOVE 'Y'                TO WS-MSOD-VALQUAL5-SW      GBIFPGM 
06115            ELSE                                                   GBIFPGM 
06116               MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)               GBIFPGM 
07709                                      TO  WS-VALUE-LIMIT           GBIFPGM 
07710               MOVE WS-VALUE-LIMIT    TO GCBH-MSA-SANC-DED-OUT     GBIFPGM 
07711            END-IF                                                 GBIFPGM 
07712         END-IF                                                    GBIFPGM 
07713      ELSE                                                         GBIFPGM 
07714      IF GAC-DEDL-ACCUMID (GAC-INDEX) = 'MSAD'                     GBIFPGM 
07715         IF GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'             GBIFPGM 
07716            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
07717                                       TO  WS-VALUE-LIMIT-S        GBIFPGM 
07718            MOVE WS-VALUE-LIMIT-S      TO GCBH-MSA-SANC-DED-OTH    GBIFPGM 
07719            MOVE 'Y'                   TO WS-MSAD-VALQUAL5-SW      GBIFPGM 
07720         ELSE                                                      GBIFPGM 
07721            MOVE GAC-DEDL-VALUE-LIMIT (GAC-INDEX)                  GBIFPGM 
07722                                       TO  WS-VALUE-LIMIT          GBIFPGM 
07723            MOVE WS-VALUE-LIMIT        TO GCBH-MSA-SANC-DED-OTH    GBIFPGM 
07724         END-IF.                                                   GBIFPGM 
06119                                                                   GBIFPGM 
06120  4350-EXIT.                                                       GBIFPGM 
06121      EXIT.                                                        GBIFPGM 
06122 /                                                                 GBIFPGM 
06123 * MQ 04/07/04                                                     GBIFPGM 
06124  4350A-BLD-MSA-SANCTION-DED.                                      GBIFPGM 
06125                                                                   GBIFPGM 
07732      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'MSID'                    GBIFPGM 
07733         IF WS-PROCESS-CON                                         GBIFPGM 
07734            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
07735               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
07736                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
07737               MOVE WS-VALUE-LIMIT-S      TO GCBH-MSA-SANC-DED-IN  GBIFPGM 
07738            ELSE                                                   GBIFPGM 
07739               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
07740                                          TO  WS-VALUE-LIMIT       GBIFPGM 
07741               MOVE WS-VALUE-LIMIT        TO GCBH-MSA-SANC-DED-IN  GBIFPGM 
07742            END-IF                                                 GBIFPGM 
07743         END-IF                                                    GBIFPGM 
07744      END-IF                                                       GBIFPGM 
07745      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'MSOD'                    GBIFPGM 
07746         IF WS-PROCESS-GRP                                         GBIFPGM 
07747            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
07748               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
07749                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
07750               MOVE WS-VALUE-LIMIT-S      TO GCBH-MSA-SANC-DED-OUT GBIFPGM 
07751            ELSE                                                   GBIFPGM 
07752               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
07753                                          TO  WS-VALUE-LIMIT       GBIFPGM 
07754               MOVE WS-VALUE-LIMIT        TO GCBH-MSA-SANC-DED-OUT GBIFPGM 
07755            END-IF                                                 GBIFPGM 
07756         END-IF                                                    GBIFPGM 
07757      END-IF                                                       GBIFPGM 
06126      IF GAD-O-P-X-ACCUMID (GAD-INDEX) = 'MSAD'                    GBIFPGM 
06127         IF WS-PROCESS-GRP                                         GBIFPGM 
06128            IF GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX) = '5'         GBIFPGM 
06129               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
06130                                          TO  WS-VALUE-LIMIT-S     GBIFPGM 
06131               MOVE WS-VALUE-LIMIT-S      TO GCBH-MSA-SANC-DED-OTH GBIFPGM 
06132            ELSE                                                   GBIFPGM 
06133               MOVE GAD-O-P-X-VALUE-LIMIT (GAD-INDEX)              GBIFPGM 
06134                                          TO  WS-VALUE-LIMIT       GBIFPGM 
07767               MOVE WS-VALUE-LIMIT        TO GCBH-MSA-SANC-DED-OTH GBIFPGM 
07768            END-IF                                                 GBIFPGM 
07769         END-IF                                                    GBIFPGM 
07770      END-IF.                                                      GBIFPGM 
07771                                                                   GBIFPGM 
06136                                                                   GBIFPGM 
06137  4350-EXIT.                                                       GBIFPGM 
06138      EXIT.                                                        GBIFPGM 
06139 /                                                                 GBIFPGM 
06140 ******************************************************************GBIFPGM 
06141 *                                                                 GBIFPGM 
06142 *    IP MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL                    GBIFPGM 
06143 *                                                                 GBIFPGM 
06144 ******************************************************************GBIFPGM 
06145  4360-BLD-MS-ABUSE-PMT-LVL.                                       GBIFPGM 
06146                                                                   GBIFPGM 
06147      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'IPSI'                    GBIFPGM 
06148         IF WS-PROCESS-CON                                         GBIFPGM 
06149            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
06150                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
06151            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06152              TO WS-PERCENT-VAL                                    GBIFPGM 
06153            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06154                                       TO WS-PERCENT-VAL           GBIFPGM 
06155            MOVE WS-PERCENT            TO GCBH-IP-MSA-PMT-LVL-IN   GBIFPGM 
06156         END-IF                                                    GBIFPGM 
06157      END-IF                                                       GBIFPGM 
06158      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'IPSO'                    GBIFPGM 
06159         IF WS-PROCESS-GRP                                         GBIFPGM 
06160            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
06161                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
06162            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06163                                       TO WS-PERCENT-VAL           GBIFPGM 
06164            MOVE WS-PERCENT            TO GCBH-IP-MSA-PMT-LVL-OUT  GBIFPGM 
06165         END-IF                                                    GBIFPGM 
06166      END-IF                                                       GBIFPGM 
06167      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'IPSA'                    GBIFPGM 
06168         IF WS-PROCESS-CON                                         GBIFPGM 
06169            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
06170                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
06171            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06172                                          TO WS-PERCENT-VAL        GBIFPGM 
06173            MOVE WS-PERCENT            TO GCBH-IP-MSA-PMT-LVL-OTH  GBIFPGM 
06174         END-IF                                                    GBIFPGM 
06175      END-IF.                                                      GBIFPGM 
06176                                                                   GBIFPGM 
06177  4360-EXIT.                                                       GBIFPGM 
06178      EXIT.                                                        GBIFPGM 
06179 /                                                                 GBIFPGM 
06180 *MQ 10/03                                                         GBIFPGM 
06181 ******************************************************************GBIFPGM 
06182 *                                                                 GBIFPGM 
06183 *    HEARING AID BENEFIT PERIOD MAXIMUM                           GBIFPGM 
06184 *                                                                 GBIFPGM 
06185 ******************************************************************GBIFPGM 
06186  4370-BLD-HEAR-AID-BP-MAX.                                        GBIFPGM 
06187                                                                   GBIFPGM 
06188      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HAMI'                     GBIFPGM 
06189         IF WS-PROCESS-CON                                         GBIFPGM 
06190            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06191            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-HEAR-AID-BP-MAX-IN    GBIFPGM 
06192         END-IF                                                    GBIFPGM 
06193      END-IF                                                       GBIFPGM 
06194      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HAMO'                     GBIFPGM 
06195         IF WS-PROCESS-GRP                                         GBIFPGM 
06196            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06197            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-HEAR-AID-BP-MAX-OUT   GBIFPGM 
06198         END-IF                                                    GBIFPGM 
06199      END-IF                                                       GBIFPGM 
06200      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HAMA'                     GBIFPGM 
06201         IF WS-PROCESS-CON                                         GBIFPGM 
06202            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06203            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-HEAR-AID-BP-MAX-OTH   GBIFPGM 
06204         END-IF                                                    GBIFPGM 
06205      END-IF.                                                      GBIFPGM 
06206                                                                   GBIFPGM 
06207  4370-EXIT.                                                       GBIFPGM 
06208      EXIT.                                                        GBIFPGM 
06209 /                                                                 GBIFPGM 
06210 *MQ 10/03                                                         GBIFPGM 
06211 ******************************************************************GBIFPGM 
06212 *                                                                 GBIFPGM 
06213 *    NON PLAN PAYMENT LEVEL                                       GBIFPGM 
06214 *                                                                 GBIFPGM 
06215 ******************************************************************GBIFPGM 
06216  4380-BLD-NON-PLAN-PMT-LVL.                                       GBIFPGM 
06217                                                                   GBIFPGM 
06218      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'NONA'                    GBIFPGM 
06219         IF WS-PROCESS-CON                                         GBIFPGM 
06220            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
06221                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
06222            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06223              TO WS-PERCENT-VAL                                    GBIFPGM 
06224            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06225                                       TO WS-PERCENT-VAL           GBIFPGM 
06226            MOVE WS-PERCENT            TO GCBH-NON-PLAN-PMT-LVL-OTHGBIFPGM 
06227         END-IF                                                    GBIFPGM 
06228      END-IF                                                       GBIFPGM 
06229      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'NONA'                    GBIFPGM 
06230         IF WS-PROCESS-GRP                                         GBIFPGM 
06231            MOVE GAB-COINS-VALUE-LIMIT (GAB-INDEX)                 GBIFPGM 
06232                                       TO WS-VALUE-LIMIT-S         GBIFPGM 
06233            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06234                                       TO WS-PERCENT-VAL           GBIFPGM 
06235            MOVE WS-PERCENT            TO GCBH-NON-PLAN-PMT-LVL-OTHGBIFPGM 
06236         END-IF                                                    GBIFPGM 
06237      END-IF.                                                      GBIFPGM 
06238                                                                   GBIFPGM 
06239  4380-EXIT.                                                       GBIFPGM 
06240      EXIT.                                                        GBIFPGM 
06241 /                                                                 GBIFPGM 
06242 *MQ 10/03                                                         GBIFPGM 
06243 ******************************************************************GBIFPGM 
06244 *                                                                 GBIFPGM 
06245 *    CONTACT LENSES BENEFIT PERIOD MAXIMUM                        GBIFPGM 
06246 *                                                                 GBIFPGM 
06247 ******************************************************************GBIFPGM 
06248  4390-BLD-CON-LENS-BP-MAX.                                        GBIFPGM 
06249                                                                   GBIFPGM 
06250      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CONA'                     GBIFPGM 
06251         IF WS-PROCESS-CON                                         GBIFPGM 
06252            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06253            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-CON-LENS-BP-MAX-OTH   GBIFPGM 
06254         END-IF                                                    GBIFPGM 
06255      END-IF                                                       GBIFPGM 
06256      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CONA'                     GBIFPGM 
06257         IF WS-PROCESS-GRP                                         GBIFPGM 
06258            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06259            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-CON-LENS-BP-MAX-OTH   GBIFPGM 
06260         END-IF                                                    GBIFPGM 
06261      END-IF.                                                      GBIFPGM 
06262                                                                   GBIFPGM 
06263  4390-EXIT.                                                       GBIFPGM 
06264      EXIT.                                                        GBIFPGM 
06265 /                                                                 GBIFPGM 
06266 *MQ 10/03                                                         GBIFPGM 
06267 ******************************************************************GBIFPGM 
06268 *                                                                 GBIFPGM 
06269 *    FRAMES BENEFIT PERIOD MAXIMUM                                GBIFPGM 
06270 *                                                                 GBIFPGM 
06271 ******************************************************************GBIFPGM 
06272  4392-BLD-FRAME-BP-MAX.                                           GBIFPGM 
06273                                                                   GBIFPGM 
06274      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'FRAA'                     GBIFPGM 
06275         IF WS-PROCESS-CON                                         GBIFPGM 
06276            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06277            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-FRAME-BP-MAX-OTH      GBIFPGM 
06278         END-IF                                                    GBIFPGM 
06279      END-IF                                                       GBIFPGM 
06280      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'FRAA'                     GBIFPGM 
06281         IF WS-PROCESS-GRP                                         GBIFPGM 
06282            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06283            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-FRAME-BP-MAX-OTH      GBIFPGM 
06284         END-IF                                                    GBIFPGM 
06285      END-IF.                                                      GBIFPGM 
06286                                                                   GBIFPGM 
06287  4392-EXIT.                                                       GBIFPGM 
06288      EXIT.                                                        GBIFPGM 
06289 /                                                                 GBIFPGM 
06290 *MQ 10/03                                                         GBIFPGM 
06291 ******************************************************************GBIFPGM 
06292 *                                                                 GBIFPGM 
06293 *    VISION EXAM BENEFIT PERIOD MAXIMUM                           GBIFPGM 
06294 *                                                                 GBIFPGM 
06295 ******************************************************************GBIFPGM 
06296  4394-BLD-VIS-EX-BP-MAX.                                          GBIFPGM 
06297                                                                   GBIFPGM 
07934      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'VISI'                     GBIFPGM 
06299         IF WS-PROCESS-CON                                         GBIFPGM 
06300            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07937            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-VIS-EX-BP-MAX-IN      GBIFPGM 
06302         END-IF                                                    GBIFPGM 
06303      END-IF                                                       GBIFPGM 
07940      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'VISO'                     GBIFPGM 
07941         IF WS-PROCESS-GRP                                         GBIFPGM 
07942            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07943            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-VIS-EX-BP-MAX-OUT     GBIFPGM 
07944         END-IF                                                    GBIFPGM 
07945      END-IF                                                       GBIFPGM 
06304      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'VISA'                     GBIFPGM 
07947         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
07948         MOVE WS-TEMP-VALUE-LIMIT TO GCBH-VIS-EX-BP-MAX-OTH        GBIFPGM 
06309      END-IF.                                                      GBIFPGM 
07950                                                                   GBIFPGM 
06310                                                                   GBIFPGM 
06311  4394-EXIT.                                                       GBIFPGM 
06312      EXIT.                                                        GBIFPGM 
06313 /                                                                 GBIFPGM 
06314 *MQ 10/03                                                         GBIFPGM 
06315 ******************************************************************GBIFPGM 
06316 *                                                                 GBIFPGM 
06317 *    LENSES BENEFIT PERIOD MAXIMUM                                GBIFPGM 
06318 *                                                                 GBIFPGM 
06319 ******************************************************************GBIFPGM 
06320  4396-BLD-LENS-BP-MAX.                                            GBIFPGM 
06321                                                                   GBIFPGM 
06322      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'LENA'                     GBIFPGM 
06323         IF WS-PROCESS-CON                                         GBIFPGM 
06324            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06325            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-LENS-BP-MAX-OTH       GBIFPGM 
06326         END-IF                                                    GBIFPGM 
06327      END-IF                                                       GBIFPGM 
06328      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'LENA'                     GBIFPGM 
06329         IF WS-PROCESS-GRP                                         GBIFPGM 
06330            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06331            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-LENS-BP-MAX-OTH       GBIFPGM 
06332         END-IF                                                    GBIFPGM 
06333      END-IF.                                                      GBIFPGM 
06334                                                                   GBIFPGM 
06335  4396-EXIT.                                                       GBIFPGM 
06336      EXIT.                                                        GBIFPGM 
06337 /                                                                 GBIFPGM 
06338 *MQ 04/07/04                                                      GBIFPGM 
06339 ******************************************************************GBIFPGM 
06340 *                                                                 GBIFPGM 
06341 *    VISION HARWDWARE BENEFIT PERIOD MAXIMUM                      GBIFPGM 
06342 *                                                                 GBIFPGM 
06343 ******************************************************************GBIFPGM 
06344  4398-BLD-VIS-HW-BP-MAX.                                          GBIFPGM 
06345                                                                   GBIFPGM 
06346      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'GLAA'                     GBIFPGM 
06347         IF WS-PROCESS-CON                                         GBIFPGM 
06348            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06349            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-VIS-HW-BP-MAX-OTH     GBIFPGM 
06350         END-IF                                                    GBIFPGM 
06351      END-IF                                                       GBIFPGM 
06352      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'GLAA'                     GBIFPGM 
06353         IF WS-PROCESS-GRP                                         GBIFPGM 
06354            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06355            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-VIS-HW-BP-MAX-OTH     GBIFPGM 
06356         END-IF                                                    GBIFPGM 
06357      END-IF.                                                      GBIFPGM 
06358                                                                   GBIFPGM 
06359  4398-EXIT.                                                       GBIFPGM 
06360      EXIT.                                                        GBIFPGM 
06361 /                                                                 GBIFPGM 
06362 ******************************************************************GBIFPGM 
06363 *                                                                 GBIFPGM 
06364 *    B/C MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL                   GBIFPGM 
06365 *                                                                 GBIFPGM 
06366 ******************************************************************GBIFPGM 
06367  4500-BLD-IP-BC-MSA-PMT-LVL.                                      GBIFPGM 
06368                                                                   GBIFPGM 
06369      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBCI'                    GBIFPGM 
06370         IF WS-PROCESS-CON                                         GBIFPGM 
06371            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06372                                       TO WS-PERCENT-VAL           GBIFPGM 
06373            MOVE WS-PERCENT            TO GCBH-IP-BC-MSA-PMT-LVL-INGBIFPGM 
06374         END-IF                                                    GBIFPGM 
06375      END-IF                                                       GBIFPGM 
06376      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBCO'                    GBIFPGM 
06377         IF WS-PROCESS-GRP                                         GBIFPGM 
06378            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06379                                       TO WS-PERCENT-VAL           GBIFPGM 
06380            MOVE WS-PERCENT         TO GCBH-IP-BC-MSA-PMT-LVL-OUT  GBIFPGM 
06381         END-IF                                                    GBIFPGM 
06382      END-IF                                                       GBIFPGM 
06383      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBCA'                    GBIFPGM 
06384         IF WS-PROCESS-CON                                         GBIFPGM 
06385            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06386                                          TO WS-PERCENT-VAL        GBIFPGM 
06387            MOVE WS-PERCENT         TO GCBH-IP-BC-MSA-PMT-LVL-OTH  GBIFPGM 
06388         END-IF                                                    GBIFPGM 
06389      END-IF.                                                      GBIFPGM 
06390                                                                   GBIFPGM 
06391  4500-EXIT.                                                       GBIFPGM 
06392      EXIT.                                                        GBIFPGM 
06393 /                                                                 GBIFPGM 
06394 ******************************************************************GBIFPGM 
06395 *                                                                 GBIFPGM 
06396 *    B/S MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL                   GBIFPGM 
06397 *                                                                 GBIFPGM 
06398 ******************************************************************GBIFPGM 
06399  4510-BLD-IP-BS-MSA-PMT-LVL.                                      GBIFPGM 
06400                                                                   GBIFPGM 
06401      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBCI'                    GBIFPGM 
06402         IF WS-PROCESS-CON                                         GBIFPGM 
06403            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06404                                       TO WS-PERCENT-VAL           GBIFPGM 
06405            MOVE WS-PERCENT         TO GCBH-IP-BS-MSA-PMT-LVL-IN   GBIFPGM 
06406         END-IF                                                    GBIFPGM 
06407      END-IF                                                       GBIFPGM 
06408      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBCO'                    GBIFPGM 
06409         IF WS-PROCESS-GRP                                         GBIFPGM 
06410            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06411                                       TO WS-PERCENT-VAL           GBIFPGM 
06412            MOVE WS-PERCENT         TO GCBH-IP-BS-MSA-PMT-LVL-OUT  GBIFPGM 
06413         END-IF                                                    GBIFPGM 
06414      END-IF                                                       GBIFPGM 
06415      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBCA'                    GBIFPGM 
06416         IF WS-PROCESS-CON                                         GBIFPGM 
06417            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06418                                          TO WS-PERCENT-VAL        GBIFPGM 
06419            MOVE WS-PERCENT         TO GCBH-IP-BS-MSA-PMT-LVL-OTH  GBIFPGM 
06420         END-IF                                                    GBIFPGM 
06421      END-IF.                                                      GBIFPGM 
06422                                                                   GBIFPGM 
06423  4510-EXIT.                                                       GBIFPGM 
06424      EXIT.                                                        GBIFPGM 
06425 /                                                                 GBIFPGM 
06426 ******************************************************************GBIFPGM 
06427 *                                                                 GBIFPGM 
06428 *    B/C AND B/S MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL           GBIFPGM 
06429 *                                                                 GBIFPGM 
06430 ******************************************************************GBIFPGM 
06431  4520-BLD-IP-BCBS-MSA-PMT-LVL.                                    GBIFPGM 
06432                                                                   GBIFPGM 
06433      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABCI'                    GBIFPGM 
06434         IF WS-PROCESS-CON                                         GBIFPGM 
06435            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06436                                       TO WS-PERCENT-VAL           GBIFPGM 
06437            MOVE WS-PERCENT         TO GCBH-IP-BCBS-MSA-PMT-LVL-IN GBIFPGM 
06438         END-IF                                                    GBIFPGM 
06439      END-IF                                                       GBIFPGM 
06440      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABCO'                    GBIFPGM 
06441         IF WS-PROCESS-GRP                                         GBIFPGM 
06442            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06443                                       TO WS-PERCENT-VAL           GBIFPGM 
06444            MOVE WS-PERCENT         TO GCBH-IP-BCBS-MSA-PMT-LVL-OUTGBIFPGM 
06445         END-IF                                                    GBIFPGM 
06446      END-IF                                                       GBIFPGM 
06447      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABCA'                    GBIFPGM 
06448         IF WS-PROCESS-CON                                         GBIFPGM 
06449            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06450                                          TO WS-PERCENT-VAL        GBIFPGM 
06451            MOVE WS-PERCENT       TO GCBH-IP-BCBS-MSA-PMT-LVL-OTH  GBIFPGM 
06452         END-IF                                                    GBIFPGM 
06453      END-IF.                                                      GBIFPGM 
06454                                                                   GBIFPGM 
06455  4520-EXIT.                                                       GBIFPGM 
06456      EXIT.                                                        GBIFPGM 
06457 /                                                                 GBIFPGM 
06458 ******************************************************************GBIFPGM 
06459 *                                                                 GBIFPGM 
06460 *    B/C MENTAL AND S/A COMBINED BENEFIT PERIOD MAXIMUM           GBIFPGM 
06461 *                                                                 GBIFPGM 
06462 ******************************************************************GBIFPGM 
06463  4530-BLD-IP-BC-MSA-BP-MAX.                                       GBIFPGM 
06464                                                                   GBIFPGM 
06465      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBMI'                     GBIFPGM 
06466         IF WS-PROCESS-CON                                         GBIFPGM 
06467            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06468            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MSA-BP-MAX-IN   GBIFPGM 
06469         END-IF                                                    GBIFPGM 
06470      END-IF                                                       GBIFPGM 
06471      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBMO'                     GBIFPGM 
06472         IF WS-PROCESS-GRP                                         GBIFPGM 
06473            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06474            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MSA-BP-MAX-OUT  GBIFPGM 
06475         END-IF                                                    GBIFPGM 
06476      END-IF                                                       GBIFPGM 
06477      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBMA'                     GBIFPGM 
06478         IF WS-PROCESS-CON                                         GBIFPGM 
06479            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06480            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MSA-BP-MAX-OTH  GBIFPGM 
06481         END-IF                                                    GBIFPGM 
06482      END-IF.                                                      GBIFPGM 
06483                                                                   GBIFPGM 
06484  4530-EXIT.                                                       GBIFPGM 
06485      EXIT.                                                        GBIFPGM 
06486 /                                                                 GBIFPGM 
06487 ******************************************************************GBIFPGM 
06488 *                                                                 GBIFPGM 
06489 *    B/S MENTAL AND S/A COMBINED BENEFIT PERIOD MAXIMUM           GBIFPGM 
06490 *                                                                 GBIFPGM 
06491 ******************************************************************GBIFPGM 
06492  4540-BLD-IP-BS-MSA-BP-MAX.                                       GBIFPGM 
06493                                                                   GBIFPGM 
06494      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBMI'                     GBIFPGM 
06495         IF WS-PROCESS-CON                                         GBIFPGM 
06496            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06497            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MSA-BP-MAX-IN   GBIFPGM 
06498         END-IF                                                    GBIFPGM 
06499      END-IF                                                       GBIFPGM 
06500      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBMO'                     GBIFPGM 
06501         IF WS-PROCESS-GRP                                         GBIFPGM 
06502            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06503            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MSA-BP-MAX-OUT  GBIFPGM 
06504         END-IF                                                    GBIFPGM 
06505      END-IF                                                       GBIFPGM 
06506      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBMA'                     GBIFPGM 
06507         IF WS-PROCESS-CON                                         GBIFPGM 
06508            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06509            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MSA-BP-MAX-OTH  GBIFPGM 
06510         END-IF                                                    GBIFPGM 
06511      END-IF.                                                      GBIFPGM 
06512                                                                   GBIFPGM 
06513  4540-EXIT.                                                       GBIFPGM 
06514      EXIT.                                                        GBIFPGM 
06515 /                                                                 GBIFPGM 
06516 ******************************************************************GBIFPGM 
06517 *                                                                 GBIFPGM 
06518 *    B/C AND B/S MENTAL AND SA BENEFIT PERIOD MAXIMUM             GBIFPGM 
06519 *                                                                 GBIFPGM 
06520 ******************************************************************GBIFPGM 
06521  4550-BLD-IP-BCBS-BP-MAX.                                         GBIFPGM 
06522                                                                   GBIFPGM 
06523      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABMI'                     GBIFPGM 
06524         IF WS-PROCESS-CON                                         GBIFPGM 
06525            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06526            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-MSA-BP-MAX-IN GBIFPGM 
06527         END-IF                                                    GBIFPGM 
06528      END-IF                                                       GBIFPGM 
06529      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABMO'                     GBIFPGM 
06530         IF WS-PROCESS-GRP                                         GBIFPGM 
06531            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06532            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-MSA-BP-MAX-OUTGBIFPGM 
06533         END-IF                                                    GBIFPGM 
06534      END-IF                                                       GBIFPGM 
06535      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABMA'                     GBIFPGM 
06536         IF WS-PROCESS-CON                                         GBIFPGM 
06537            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06538            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
06539                               TO GCBH-IP-BCBS-MSA-BP-MAX-OTH      GBIFPGM 
06540         END-IF                                                    GBIFPGM 
06541      END-IF.                                                      GBIFPGM 
06542                                                                   GBIFPGM 
06543  4550-EXIT.                                                       GBIFPGM 
06544      EXIT.                                                        GBIFPGM 
06545 /                                                                 GBIFPGM 
06546 ******************************************************************GBIFPGM 
06547 *                                                                 GBIFPGM 
06548 *    B/C MENTAL / SUBSTANCE ABUSE LIFETIME MAXIMUM                GBIFPGM 
06549 *                                                                 GBIFPGM 
06550 ******************************************************************GBIFPGM 
06551  4560-BLD-IP-BC-MSA-LIFE-MAX.                                     GBIFPGM 
06552                                                                   GBIFPGM 
06553      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBLI'                     GBIFPGM 
06554         IF WS-PROCESS-CON                                         GBIFPGM 
06555            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06556            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MSA-LIFE-MAX-IN GBIFPGM 
06557         END-IF                                                    GBIFPGM 
06558      END-IF                                                       GBIFPGM 
06559      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBLO'                     GBIFPGM 
06560         IF WS-PROCESS-GRP                                         GBIFPGM 
06561            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06562            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MSA-LIFE-MAX-OUTGBIFPGM 
06563         END-IF                                                    GBIFPGM 
06564      END-IF                                                       GBIFPGM 
06565      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBLA'                     GBIFPGM 
06566         IF WS-PROCESS-CON                                         GBIFPGM 
06567            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06568            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
06569                               TO GCBH-IP-BC-MSA-LIFE-MAX-OTH      GBIFPGM 
06570         END-IF                                                    GBIFPGM 
06571      END-IF.                                                      GBIFPGM 
06572                                                                   GBIFPGM 
06573  4560-EXIT.                                                       GBIFPGM 
06574      EXIT.                                                        GBIFPGM 
06575 /                                                                 GBIFPGM 
06576 ******************************************************************GBIFPGM 
06577 *                                                                 GBIFPGM 
06578 *    B/S MENTAL / SUBSTANCE ABUSE LIFETIME MAXIMUM                GBIFPGM 
06579 *                                                                 GBIFPGM 
06580 ******************************************************************GBIFPGM 
06581  4570-BLD-IP-BS-MSA-LIFE-MAX.                                     GBIFPGM 
06582                                                                   GBIFPGM 
06583      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBLI'                     GBIFPGM 
06584         IF WS-PROCESS-CON                                         GBIFPGM 
06585            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06586            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MSA-LIFE-MAX-IN GBIFPGM 
06587         END-IF                                                    GBIFPGM 
06588      END-IF                                                       GBIFPGM 
06589      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBLO'                     GBIFPGM 
06590         IF WS-PROCESS-GRP                                         GBIFPGM 
06591            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06592            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MSA-LIFE-MAX-OUTGBIFPGM 
06593         END-IF                                                    GBIFPGM 
06594      END-IF                                                       GBIFPGM 
06595      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBLA'                     GBIFPGM 
06596         IF WS-PROCESS-CON                                         GBIFPGM 
06597            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06598            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
06599                               TO GCBH-IP-BS-MSA-LIFE-MAX-OTH      GBIFPGM 
06600         END-IF                                                    GBIFPGM 
06601      END-IF.                                                      GBIFPGM 
06602                                                                   GBIFPGM 
06603  4570-EXIT.                                                       GBIFPGM 
06604      EXIT.                                                        GBIFPGM 
06605 /                                                                 GBIFPGM 
06606 ******************************************************************GBIFPGM 
06607 *                                                                 GBIFPGM 
06608 *    B/C AND B/S MENTAL / SA LIFETIME MAXIMUM                     GBIFPGM 
06609 *                                                                 GBIFPGM 
06610 ******************************************************************GBIFPGM 
06611  4580-BLD-IP-BCBS-MSA-LIFE-MAX.                                   GBIFPGM 
06612                                                                   GBIFPGM 
06613      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABLI'                     GBIFPGM 
06614         IF WS-PROCESS-CON                                         GBIFPGM 
06615            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06616            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
06617                               TO GCBH-IP-BCBS-MSA-LIFE-MAX-IN     GBIFPGM 
06618         END-IF                                                    GBIFPGM 
06619      END-IF                                                       GBIFPGM 
06620      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABLO'                     GBIFPGM 
06621         IF WS-PROCESS-GRP                                         GBIFPGM 
06622            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06623            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
06624                               TO GCBH-IP-BCBS-MSA-LIFE-MAX-OUT    GBIFPGM 
06625         END-IF                                                    GBIFPGM 
06626      END-IF                                                       GBIFPGM 
06627      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABLA'                     GBIFPGM 
06628         IF WS-PROCESS-CON                                         GBIFPGM 
06629            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06630            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
06631                               TO GCBH-IP-BCBS-MSA-LIFE-MAX-OTH    GBIFPGM 
06632         END-IF                                                    GBIFPGM 
06633      END-IF.                                                      GBIFPGM 
06634                                                                   GBIFPGM 
06635  4580-EXIT.                                                       GBIFPGM 
06636      EXIT.                                                        GBIFPGM 
06637 /                                                                 GBIFPGM 
06638 ******************************************************************GBIFPGM 
06639 *                                                                 GBIFPGM 
06640 *    B/C MENTAL HEALTH PAYMENT LEVEL                              GBIFPGM 
06641 *                                                                 GBIFPGM 
06642 ******************************************************************GBIFPGM 
06643  4590-BLD-IP-BC-MH-PMT-LVL.                                       GBIFPGM 
06644                                                                   GBIFPGM 
06645 *MQ 6/05/03                                                       GBIFPGM 
06646      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMPI'                    GBIFPGM 
06647         IF WS-PROCESS-CON                                         GBIFPGM 
06648            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06649                                       TO WS-PERCENT-VAL           GBIFPGM 
06650            MOVE WS-PERCENT         TO GCBH-IP-BC-MH-PMT-LVL-IN    GBIFPGM 
06651         END-IF                                                    GBIFPGM 
06652      END-IF                                                       GBIFPGM 
06653      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMPO'                    GBIFPGM 
06654         IF WS-PROCESS-GRP                                         GBIFPGM 
06655            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06656                                       TO WS-PERCENT-VAL           GBIFPGM 
06657            MOVE WS-PERCENT         TO GCBH-IP-BC-MH-PMT-LVL-OUT   GBIFPGM 
06658         END-IF                                                    GBIFPGM 
06659      END-IF                                                       GBIFPGM 
06660      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMPA'                    GBIFPGM 
06661         IF WS-PROCESS-CON                                         GBIFPGM 
06662            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06663                                          TO WS-PERCENT-VAL        GBIFPGM 
06664            MOVE WS-PERCENT         TO GCBH-IP-BC-MH-PMT-LVL-OTH   GBIFPGM 
06665         END-IF                                                    GBIFPGM 
06666      END-IF.                                                      GBIFPGM 
06667                                                                   GBIFPGM 
06668  4590-EXIT.                                                       GBIFPGM 
06669      EXIT.                                                        GBIFPGM 
06670 /                                                                 GBIFPGM 
06671 ******************************************************************GBIFPGM 
06672 *                                                                 GBIFPGM 
06673 *    B/S MENTAL HEALTH PAYMENT LEVEL                              GBIFPGM 
06674 *                                                                 GBIFPGM 
06675 ******************************************************************GBIFPGM 
06676  4600-BLD-IP-BS-MH-PMT-LVL.                                       GBIFPGM 
06677                                                                   GBIFPGM 
06678 *MQ 6/05/03                                                       GBIFPGM 
06679      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMPI'                    GBIFPGM 
06680         IF WS-PROCESS-CON                                         GBIFPGM 
06681            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06682                                       TO WS-PERCENT-VAL           GBIFPGM 
06683            MOVE WS-PERCENT         TO GCBH-IP-BS-MH-PMT-LVL-IN    GBIFPGM 
06684         END-IF                                                    GBIFPGM 
06685      END-IF                                                       GBIFPGM 
06686      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMPO'                    GBIFPGM 
06687         IF WS-PROCESS-GRP                                         GBIFPGM 
06688            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06689                                       TO WS-PERCENT-VAL           GBIFPGM 
06690            MOVE WS-PERCENT         TO GCBH-IP-BS-MH-PMT-LVL-OUT   GBIFPGM 
06691         END-IF                                                    GBIFPGM 
06692      END-IF                                                       GBIFPGM 
06693      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMPA'                    GBIFPGM 
06694         IF WS-PROCESS-CON                                         GBIFPGM 
06695            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06696                                          TO WS-PERCENT-VAL        GBIFPGM 
06697            MOVE WS-PERCENT         TO GCBH-IP-BS-MH-PMT-LVL-OTH   GBIFPGM 
06698         END-IF                                                    GBIFPGM 
06699      END-IF.                                                      GBIFPGM 
06700                                                                   GBIFPGM 
06701  4600-EXIT.                                                       GBIFPGM 
06702      EXIT.                                                        GBIFPGM 
06703 /                                                                 GBIFPGM 
06704 ******************************************************************GBIFPGM 
06705 *                                                                 GBIFPGM 
06706 *    B/C AND B/S MENTAL HEALTH PAYMENT LEVEL                      GBIFPGM 
06707 *                                                                 GBIFPGM 
06708 ******************************************************************GBIFPGM 
06709  4610-BLD-IP-BCBS-MH-PMT-LVL.                                     GBIFPGM 
06710                                                                   GBIFPGM 
06711 *MQ 6/05/03                                                       GBIFPGM 
06712      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMPI'                    GBIFPGM 
06713         IF WS-PROCESS-CON                                         GBIFPGM 
06714            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06715                                       TO WS-PERCENT-VAL           GBIFPGM 
06716            MOVE WS-PERCENT       TO GCBH-IP-BCBS-MH-PMT-LVL-IN    GBIFPGM 
06717         END-IF                                                    GBIFPGM 
06718      END-IF                                                       GBIFPGM 
06719      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMPO'                    GBIFPGM 
06720         IF WS-PROCESS-GRP                                         GBIFPGM 
06721            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06722                                       TO WS-PERCENT-VAL           GBIFPGM 
06723            MOVE WS-PERCENT       TO GCBH-IP-BCBS-MH-PMT-LVL-OUT   GBIFPGM 
06724         END-IF                                                    GBIFPGM 
06725      END-IF                                                       GBIFPGM 
06726      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMPA'                    GBIFPGM 
06727         IF WS-PROCESS-CON                                         GBIFPGM 
06728            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06729                                          TO WS-PERCENT-VAL        GBIFPGM 
06730            MOVE WS-PERCENT       TO GCBH-IP-BCBS-MH-PMT-LVL-OTH   GBIFPGM 
06731         END-IF                                                    GBIFPGM 
06732      END-IF.                                                      GBIFPGM 
06733                                                                   GBIFPGM 
06734  4610-EXIT.                                                       GBIFPGM 
06735      EXIT.                                                        GBIFPGM 
06736 /                                                                 GBIFPGM 
06737 ******************************************************************GBIFPGM 
06738 *                                                                 GBIFPGM 
06739 *    B/C MENTAL HEALTH BENEFIT PERIOD MAXIMUM                     GBIFPGM 
06740 *                                                                 GBIFPGM 
06741 ******************************************************************GBIFPGM 
06742  4620-BLD-IP-BC-MH-BP-MAX.                                        GBIFPGM 
06743                                                                   GBIFPGM 
06744      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMMI'                     GBIFPGM 
06745         IF WS-PROCESS-CON                                         GBIFPGM 
06746            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06747            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MH-BP-MAX-IN    GBIFPGM 
06748         END-IF                                                    GBIFPGM 
06749      END-IF                                                       GBIFPGM 
06750      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMMO'                     GBIFPGM 
06751         IF WS-PROCESS-GRP                                         GBIFPGM 
06752            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06753            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MH-BP-MAX-OUT   GBIFPGM 
06754         END-IF                                                    GBIFPGM 
06755      END-IF                                                       GBIFPGM 
06756      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMMA'                     GBIFPGM 
06757         IF WS-PROCESS-CON                                         GBIFPGM 
06758            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06759            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MH-BP-MAX-OTH   GBIFPGM 
06760         END-IF                                                    GBIFPGM 
06761      END-IF.                                                      GBIFPGM 
06762                                                                   GBIFPGM 
06763  4620-EXIT.                                                       GBIFPGM 
06764      EXIT.                                                        GBIFPGM 
06765 /                                                                 GBIFPGM 
06766 ******************************************************************GBIFPGM 
06767 *                                                                 GBIFPGM 
08409 *    B/C MENTAL HEALTH BENEFIT PERIOD MAXIMUM - ALL POT           GBIFPGM 
08410 *                                                                 GBIFPGM 
08411 ******************************************************************GBIFPGM 
08412  4625-BLD-BC-MH-BP-MAX-POT.                                       GBIFPGM 
08413                                                                   GBIFPGM 
08414      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMDI'                     GBIFPGM 
08415         IF WS-PROCESS-CON                                         GBIFPGM 
08416            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08417            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-MH-BP-MAX-POT-IN   GBIFPGM 
08418         END-IF                                                    GBIFPGM 
08419      END-IF                                                       GBIFPGM 
08420      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMDO'                     GBIFPGM 
08421         IF WS-PROCESS-GRP                                         GBIFPGM 
08422            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08423            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-MH-BP-MAX-POT-OUT  GBIFPGM 
08424         END-IF                                                    GBIFPGM 
08425      END-IF                                                       GBIFPGM 
08426      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMDA'                     GBIFPGM 
08427         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
08428         MOVE WS-TEMP-VALUE-LIMIT    TO GCBH-BC-MH-BP-MAX-POT-OTH  GBIFPGM 
08429      END-IF.                                                      GBIFPGM 
08430                                                                   GBIFPGM 
08431  4625-EXIT.                                                       GBIFPGM 
08432      EXIT.                                                        GBIFPGM 
08433 /                                                                 GBIFPGM 
08434 ******************************************************************GBIFPGM 
08435 *                                                                 GBIFPGM 
06768 *    B/S MENTAL HEALTH BENEFIT PERIOD MAXIMUM                     GBIFPGM 
06769 *                                                                 GBIFPGM 
06770 ******************************************************************GBIFPGM 
06771  4630-BLD-IP-BS-MH-BP-MAX.                                        GBIFPGM 
06772                                                                   GBIFPGM 
06773      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMMI'                     GBIFPGM 
06774         IF WS-PROCESS-CON                                         GBIFPGM 
06775            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06776            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MH-BP-MAX-IN    GBIFPGM 
06777         END-IF                                                    GBIFPGM 
06778      END-IF                                                       GBIFPGM 
06779      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMMO'                     GBIFPGM 
06780         IF WS-PROCESS-GRP                                         GBIFPGM 
06781            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06782            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MH-BP-MAX-OUT   GBIFPGM 
06783         END-IF                                                    GBIFPGM 
06784      END-IF                                                       GBIFPGM 
06785      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMMA'                     GBIFPGM 
06786         IF WS-PROCESS-CON                                         GBIFPGM 
06787            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06788            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MH-BP-MAX-OTH   GBIFPGM 
06789         END-IF                                                    GBIFPGM 
06790      END-IF.                                                      GBIFPGM 
06791                                                                   GBIFPGM 
06792  4630-EXIT.                                                       GBIFPGM 
06793      EXIT.                                                        GBIFPGM 
06794 /                                                                 GBIFPGM 
06795 ******************************************************************GBIFPGM 
06796 *                                                                 GBIFPGM 
08465 *    B/S MENTAL HEALTH BENEFIT PERIOD MAXIMUM - ALL POT           GBIFPGM 
08466 *                                                                 GBIFPGM 
08467 ******************************************************************GBIFPGM 
08468  4635-BLD-BS-MH-BP-MAX-POT.                                       GBIFPGM 
08469                                                                   GBIFPGM 
08470      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMDI'                     GBIFPGM 
08471         IF WS-PROCESS-CON                                         GBIFPGM 
08472            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08473            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-MH-BP-MAX-POT-IN   GBIFPGM 
08474         END-IF                                                    GBIFPGM 
08475      END-IF                                                       GBIFPGM 
08476      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMDO'                     GBIFPGM 
08477         IF WS-PROCESS-GRP                                         GBIFPGM 
08478            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08479            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-MH-BP-MAX-POT-OUT  GBIFPGM 
08480         END-IF                                                    GBIFPGM 
08481      END-IF                                                       GBIFPGM 
08482      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMDA'                     GBIFPGM 
08483         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
08484         MOVE WS-TEMP-VALUE-LIMIT    TO GCBH-BS-MH-BP-MAX-POT-OTH  GBIFPGM 
08485      END-IF.                                                      GBIFPGM 
08486                                                                   GBIFPGM 
08487  4635-EXIT.                                                       GBIFPGM 
08488      EXIT.                                                        GBIFPGM 
08489 /                                                                 GBIFPGM 
08490 ******************************************************************GBIFPGM 
08491 *                                                                 GBIFPGM 
06797 *    B/C AND B/S MENTAL HEALTH BENEFIT PERIOD MAXIMUM             GBIFPGM 
06798 *                                                                 GBIFPGM 
06799 ******************************************************************GBIFPGM 
06800  4640-BLD-IP-BCBS-MH-BP-MAX.                                      GBIFPGM 
06801                                                                   GBIFPGM 
06802      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMMI'                     GBIFPGM 
06803         IF WS-PROCESS-CON                                         GBIFPGM 
06804            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06805            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-MH-BP-MAX-IN  GBIFPGM 
06806         END-IF                                                    GBIFPGM 
06807      END-IF                                                       GBIFPGM 
06808      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMMO'                     GBIFPGM 
06809         IF WS-PROCESS-GRP                                         GBIFPGM 
06810            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06811            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-MH-BP-MAX-OUT GBIFPGM 
06812         END-IF                                                    GBIFPGM 
06813      END-IF                                                       GBIFPGM 
06814      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMMA'                     GBIFPGM 
06815         IF WS-PROCESS-CON                                         GBIFPGM 
06816            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06817            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-MH-BP-MAX-OTH GBIFPGM 
06818         END-IF                                                    GBIFPGM 
06819      END-IF.                                                      GBIFPGM 
06820                                                                   GBIFPGM 
06821  4640-EXIT.                                                       GBIFPGM 
06822      EXIT.                                                        GBIFPGM 
06823 /                                                                 GBIFPGM 
06824 ******************************************************************GBIFPGM 
06825 *                                                                 GBIFPGM 
08521 *    B/C AND B/S MENTAL HEALTH BENEFIT PERIOD MAXIMUM - ALL POT   GBIFPGM 
08522 *                                                                 GBIFPGM 
08523 ******************************************************************GBIFPGM 
08524  4645-BLD-BCBS-MH-BP-MAX-POT.                                     GBIFPGM 
08525                                                                   GBIFPGM 
08526      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMDI'                     GBIFPGM 
08527         IF WS-PROCESS-CON                                         GBIFPGM 
08528            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08529            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BCBS-MH-BP-MAX-POT-IN GBIFPGM 
08530         END-IF                                                    GBIFPGM 
08531      END-IF                                                       GBIFPGM 
08532      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMDO'                     GBIFPGM 
08533         IF WS-PROCESS-GRP                                         GBIFPGM 
08534            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08535            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BCBS-MH-BP-MAX-POT-OUTGBIFPGM 
08536         END-IF                                                    GBIFPGM 
08537      END-IF                                                       GBIFPGM 
08538      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMDA'                     GBIFPGM 
08539         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
08540         MOVE WS-TEMP-VALUE-LIMIT    TO GCBH-BCBS-MH-BP-MAX-POT-OTHGBIFPGM 
08541      END-IF.                                                      GBIFPGM 
08542                                                                   GBIFPGM 
08543  4645-EXIT.                                                       GBIFPGM 
08544      EXIT.                                                        GBIFPGM 
08545 /                                                                 GBIFPGM 
08546 ******************************************************************GBIFPGM 
08547 *                                                                 GBIFPGM 
06826 *    B/C MENTAL HEALTH LIFETIME MAXIMUM                           GBIFPGM 
06827 *                                                                 GBIFPGM 
06828 ******************************************************************GBIFPGM 
06829  4650-BLD-IP-BC-MH-LIFE-MAX.                                      GBIFPGM 
06830                                                                   GBIFPGM 
06831      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMLI'                     GBIFPGM 
06832         IF WS-PROCESS-CON                                         GBIFPGM 
06833            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06834            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MH-LIFE-MAX-IN  GBIFPGM 
06835         END-IF                                                    GBIFPGM 
06836      END-IF                                                       GBIFPGM 
06837      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMLO'                     GBIFPGM 
06838         IF WS-PROCESS-GRP                                         GBIFPGM 
06839            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06840            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MH-LIFE-MAX-OUT GBIFPGM 
06841         END-IF                                                    GBIFPGM 
06842      END-IF                                                       GBIFPGM 
06843      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMLA'                     GBIFPGM 
06844         IF WS-PROCESS-CON                                         GBIFPGM 
06845            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06846            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-MH-LIFE-MAX-OTH GBIFPGM 
06847         END-IF                                                    GBIFPGM 
06848      END-IF.                                                      GBIFPGM 
06849                                                                   GBIFPGM 
06850  4650-EXIT.                                                       GBIFPGM 
06851      EXIT.                                                        GBIFPGM 
06852 /                                                                 GBIFPGM 
06853 ******************************************************************GBIFPGM 
06854 *                                                                 GBIFPGM 
08577 *    B/C MENTAL HEALTH LIFETIME MAXIMUM - ALL POT                 GBIFPGM 
08578 *                                                                 GBIFPGM 
08579 ******************************************************************GBIFPGM 
08580  4655-BLD-BC-MH-LIFE-MAX-POT.                                     GBIFPGM 
08581                                                                   GBIFPGM 
08582      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMFI'                     GBIFPGM 
08583         IF WS-PROCESS-CON                                         GBIFPGM 
08584            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08585            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-MH-LIFE-MAX-POT-IN GBIFPGM 
08586         END-IF                                                    GBIFPGM 
08587      END-IF                                                       GBIFPGM 
08588      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMFO'                     GBIFPGM 
08589         IF WS-PROCESS-GRP                                         GBIFPGM 
08590            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08591            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-MH-LIFE-MAX-POT-OUTGBIFPGM 
08592         END-IF                                                    GBIFPGM 
08593      END-IF                                                       GBIFPGM 
08594      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMFA'                     GBIFPGM 
08595         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
08596         MOVE WS-TEMP-VALUE-LIMIT    TO GCBH-BC-MH-LIFE-MAX-POT-OTHGBIFPGM 
08597      END-IF.                                                      GBIFPGM 
08598                                                                   GBIFPGM 
08599  4655-EXIT.                                                       GBIFPGM 
08600      EXIT.                                                        GBIFPGM 
08601 /                                                                 GBIFPGM 
08602 ******************************************************************GBIFPGM 
08603 *                                                                 GBIFPGM 
06855 *    B/S MENTAL HEALTH LIFETIME MAXIMUM                           GBIFPGM 
06856 *                                                                 GBIFPGM 
06857 ******************************************************************GBIFPGM 
06858  4660-BLD-IP-BS-MH-LIFE-MAX.                                      GBIFPGM 
06859                                                                   GBIFPGM 
06860      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMLI'                     GBIFPGM 
06861         IF WS-PROCESS-CON                                         GBIFPGM 
06862            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06863            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MH-LIFE-MAX-IN  GBIFPGM 
06864         END-IF                                                    GBIFPGM 
06865      END-IF                                                       GBIFPGM 
06866      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMLO'                     GBIFPGM 
06867         IF WS-PROCESS-GRP                                         GBIFPGM 
06868            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06869            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MH-LIFE-MAX-OUT GBIFPGM 
06870         END-IF                                                    GBIFPGM 
06871      END-IF                                                       GBIFPGM 
06872      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMLA'                     GBIFPGM 
06873         IF WS-PROCESS-CON                                         GBIFPGM 
06874            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06875            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-MH-LIFE-MAX-OTH GBIFPGM 
06876         END-IF                                                    GBIFPGM 
06877      END-IF.                                                      GBIFPGM 
06878                                                                   GBIFPGM 
06879  4660-EXIT.                                                       GBIFPGM 
06880      EXIT.                                                        GBIFPGM 
06881 /                                                                 GBIFPGM 
06882 ******************************************************************GBIFPGM 
06883 *                                                                 GBIFPGM 
08633 *    B/S MENTAL HEALTH LIFETIME MAXIMUM - ALL POT                 GBIFPGM 
08634 *                                                                 GBIFPGM 
08635 ******************************************************************GBIFPGM 
08636  4665-BLD-BS-MH-LIFE-MAX-POT.                                     GBIFPGM 
08637                                                                   GBIFPGM 
08638      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMFI'                     GBIFPGM 
08639         IF WS-PROCESS-CON                                         GBIFPGM 
08640            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08641            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-MH-LIFE-MAX-POT-IN GBIFPGM 
08642         END-IF                                                    GBIFPGM 
08643      END-IF                                                       GBIFPGM 
08644      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMFO'                     GBIFPGM 
08645         IF WS-PROCESS-GRP                                         GBIFPGM 
08646            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08647            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-MH-LIFE-MAX-POT-OUTGBIFPGM 
08648         END-IF                                                    GBIFPGM 
08649      END-IF                                                       GBIFPGM 
08650      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMFA'                     GBIFPGM 
08651         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
08652         MOVE WS-TEMP-VALUE-LIMIT    TO GCBH-BS-MH-LIFE-MAX-POT-OTHGBIFPGM 
08653      END-IF.                                                      GBIFPGM 
08654                                                                   GBIFPGM 
08655  4665-EXIT.                                                       GBIFPGM 
08656      EXIT.                                                        GBIFPGM 
08657 /                                                                 GBIFPGM 
08658 ******************************************************************GBIFPGM 
08659 *                                                                 GBIFPGM 
06884 *    B/C AND B/S MENTAL HEALTH LIFETIME MAXIMUM                   GBIFPGM 
06885 *                                                                 GBIFPGM 
06886 ******************************************************************GBIFPGM 
06887  4670-BLD-IP-BCBS-MH-LIFE-MAX.                                    GBIFPGM 
06888                                                                   GBIFPGM 
06889      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMLI'                     GBIFPGM 
06890         IF WS-PROCESS-CON                                         GBIFPGM 
06891            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06892            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-MH-LIFE-MAX-INGBIFPGM 
06893         END-IF                                                    GBIFPGM 
06894      END-IF                                                       GBIFPGM 
06895      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMLO'                     GBIFPGM 
06896         IF WS-PROCESS-GRP                                         GBIFPGM 
06897            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06898            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
06899                               TO GCBH-IP-BCBS-MH-LIFE-MAX-OUT     GBIFPGM 
06900         END-IF                                                    GBIFPGM 
06901      END-IF                                                       GBIFPGM 
06902      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMLA'                     GBIFPGM 
06903         IF WS-PROCESS-CON                                         GBIFPGM 
06904            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
06905            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
06906                               TO GCBH-IP-BCBS-MH-LIFE-MAX-OTH     GBIFPGM 
06907         END-IF                                                    GBIFPGM 
06908      END-IF.                                                      GBIFPGM 
06909                                                                   GBIFPGM 
06910  4670-EXIT.                                                       GBIFPGM 
08687      EXIT.                                                        GBIFPGM 
08688 /                                                                 GBIFPGM 
08689 ******************************************************************GBIFPGM 
08690 *                                                                 GBIFPGM 
08691 *    B/C AND B/S MENTAL HEALTH LIFETIME MAXIMUM - ALL POT         GBIFPGM 
08692 *                                                                 GBIFPGM 
08693 ******************************************************************GBIFPGM 
08694  4675-BLD-BCBS-MH-LIFE-MAX-POT.                                   GBIFPGM 
08695                                                                   GBIFPGM 
08696      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMFI'                     GBIFPGM 
08697         IF WS-PROCESS-CON                                         GBIFPGM 
08698            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08699            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08700                               TO GCBH-BCBS-MH-LIFE-MAX-POT-IN     GBIFPGM 
08701         END-IF                                                    GBIFPGM 
08702      END-IF                                                       GBIFPGM 
08703      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMFO'                     GBIFPGM 
08704         IF WS-PROCESS-GRP                                         GBIFPGM 
08705            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08706            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08707                               TO GCBH-BCBS-MH-LIFE-MAX-POT-OUT    GBIFPGM 
08708         END-IF                                                    GBIFPGM 
08709      END-IF                                                       GBIFPGM 
08710      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMFA'                     GBIFPGM 
08711         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
08712         MOVE WS-TEMP-VALUE-LIMIT                                  GBIFPGM 
08713                               TO GCBH-BCBS-MH-LIFE-MAX-POT-OTH    GBIFPGM 
08714      END-IF.                                                      GBIFPGM 
08715                                                                   GBIFPGM 
08716  4675-EXIT.                                                       GBIFPGM 
06911      EXIT.                                                        GBIFPGM 
06912 /                                                                 GBIFPGM 
06913 ******************************************************************GBIFPGM 
06914 *                                                                 GBIFPGM 
06915 *    B/C SUBSTANCE ABUSE PAYMENT LEVEL                            GBIFPGM 
06916 *                                                                 GBIFPGM 
06917 ******************************************************************GBIFPGM 
06918  4680-BLD-IP-BC-SA-PMT-LVL.                                       GBIFPGM 
06919                                                                   GBIFPGM 
06920 *MQ 6/05/03                                                       GBIFPGM 
06921      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSPI'                    GBIFPGM 
06922         IF WS-PROCESS-CON                                         GBIFPGM 
06923            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06924                                       TO WS-PERCENT-VAL           GBIFPGM 
06925            MOVE WS-PERCENT       TO GCBH-IP-BC-SA-PMT-LVL-IN      GBIFPGM 
06926         END-IF                                                    GBIFPGM 
06927      END-IF                                                       GBIFPGM 
06928      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSPO'                    GBIFPGM 
06929         IF WS-PROCESS-GRP                                         GBIFPGM 
06930            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06931                                       TO WS-PERCENT-VAL           GBIFPGM 
06932            MOVE WS-PERCENT       TO GCBH-IP-BC-SA-PMT-LVL-OUT     GBIFPGM 
06933         END-IF                                                    GBIFPGM 
06934      END-IF                                                       GBIFPGM 
06935      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSPA'                    GBIFPGM 
06936         IF WS-PROCESS-CON                                         GBIFPGM 
06937            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06938                                          TO WS-PERCENT-VAL        GBIFPGM 
06939            MOVE WS-PERCENT       TO GCBH-IP-BC-SA-PMT-LVL-OTH     GBIFPGM 
06940         END-IF                                                    GBIFPGM 
06941      END-IF.                                                      GBIFPGM 
06942                                                                   GBIFPGM 
06943  4680-EXIT.                                                       GBIFPGM 
06944      EXIT.                                                        GBIFPGM 
06945 /                                                                 GBIFPGM 
06946 ******************************************************************GBIFPGM 
06947 *                                                                 GBIFPGM 
06948 *    B/S SUBSTANCE ABUSE PAYMENT LEVEL                            GBIFPGM 
06949 *                                                                 GBIFPGM 
06950 ******************************************************************GBIFPGM 
06951  4690-BLD-IP-BS-SA-PMT-LVL.                                       GBIFPGM 
06952                                                                   GBIFPGM 
06953 *MQ 6/05/03                                                       GBIFPGM 
06954      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSPI'                    GBIFPGM 
06955         IF WS-PROCESS-CON                                         GBIFPGM 
06956            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06957                                       TO WS-PERCENT-VAL           GBIFPGM 
06958            MOVE WS-PERCENT       TO GCBH-IP-BS-SA-PMT-LVL-IN      GBIFPGM 
06959         END-IF                                                    GBIFPGM 
06960      END-IF                                                       GBIFPGM 
06961      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSPO'                    GBIFPGM 
06962         IF WS-PROCESS-GRP                                         GBIFPGM 
06963            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06964                                       TO WS-PERCENT-VAL           GBIFPGM 
06965            MOVE WS-PERCENT       TO GCBH-IP-BS-SA-PMT-LVL-OUT     GBIFPGM 
06966         END-IF                                                    GBIFPGM 
06967      END-IF                                                       GBIFPGM 
06968      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSPA'                    GBIFPGM 
06969         IF WS-PROCESS-CON                                         GBIFPGM 
06970            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06971                                          TO WS-PERCENT-VAL        GBIFPGM 
06972            MOVE WS-PERCENT       TO GCBH-IP-BS-SA-PMT-LVL-OTH     GBIFPGM 
06973         END-IF                                                    GBIFPGM 
06974      END-IF.                                                      GBIFPGM 
06975                                                                   GBIFPGM 
06976  4690-EXIT.                                                       GBIFPGM 
06977      EXIT.                                                        GBIFPGM 
06978 /                                                                 GBIFPGM 
06979 ******************************************************************GBIFPGM 
06980 *                                                                 GBIFPGM 
06981 *    B/C AND B/S SUBSTANCE ABUSE PAYMENT LEVEL                    GBIFPGM 
06982 *                                                                 GBIFPGM 
06983 ******************************************************************GBIFPGM 
06984  4700-BLD-IP-BCBS-SA-PMT-LVL.                                     GBIFPGM 
06985                                                                   GBIFPGM 
06986 *MQ 6/05/03                                                       GBIFPGM 
06987      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASPI'                    GBIFPGM 
06988         IF WS-PROCESS-CON                                         GBIFPGM 
06989            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06990                                       TO WS-PERCENT-VAL           GBIFPGM 
06991            MOVE WS-PERCENT       TO GCBH-IP-BCBS-SA-PMT-LVL-IN    GBIFPGM 
06992         END-IF                                                    GBIFPGM 
06993      END-IF                                                       GBIFPGM 
06994      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASPO'                    GBIFPGM 
06995         IF WS-PROCESS-GRP                                         GBIFPGM 
06996            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
06997                                       TO WS-PERCENT-VAL           GBIFPGM 
06998            MOVE WS-PERCENT       TO GCBH-IP-BCBS-SA-PMT-LVL-OUT   GBIFPGM 
06999         END-IF                                                    GBIFPGM 
07000      END-IF                                                       GBIFPGM 
07001      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASPA'                    GBIFPGM 
07002         IF WS-PROCESS-CON                                         GBIFPGM 
07003            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07004                                          TO WS-PERCENT-VAL        GBIFPGM 
07005            MOVE WS-PERCENT       TO GCBH-IP-BCBS-SA-PMT-LVL-OTH   GBIFPGM 
07006         END-IF                                                    GBIFPGM 
07007      END-IF.                                                      GBIFPGM 
07008                                                                   GBIFPGM 
07009  4700-EXIT.                                                       GBIFPGM 
07010      EXIT.                                                        GBIFPGM 
07011 /                                                                 GBIFPGM 
07012 ******************************************************************GBIFPGM 
07013 *                                                                 GBIFPGM 
07014 *    B/C SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM                   GBIFPGM 
07015 *                                                                 GBIFPGM 
07016 ******************************************************************GBIFPGM 
07017  4710-BLD-IP-BC-SA-BEN-PERD-MAX.                                  GBIFPGM 
07018                                                                   GBIFPGM 
07019      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSMI'                     GBIFPGM 
07020         IF WS-PROCESS-CON                                         GBIFPGM 
07021            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07022            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-SA-BP-MAX-IN    GBIFPGM 
07023         END-IF                                                    GBIFPGM 
07024      END-IF                                                       GBIFPGM 
07025      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSMO'                     GBIFPGM 
07026         IF WS-PROCESS-GRP                                         GBIFPGM 
07027            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07028            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-SA-BP-MAX-OUT   GBIFPGM 
07029         END-IF                                                    GBIFPGM 
07030      END-IF                                                       GBIFPGM 
07031      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSMA'                     GBIFPGM 
07032         IF WS-PROCESS-CON                                         GBIFPGM 
07033            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07034            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-SA-BP-MAX-OTH   GBIFPGM 
07035         END-IF                                                    GBIFPGM 
07036      END-IF.                                                      GBIFPGM 
07037                                                                   GBIFPGM 
07038  4710-EXIT.                                                       GBIFPGM 
07039      EXIT.                                                        GBIFPGM 
07040 /                                                                 GBIFPGM 
07041 *MQ 10/03                                                         GBIFPGM 
07042 ******************************************************************GBIFPGM 
07043 *                                                                 GBIFPGM 
07044 *    B/C SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM - ALL POT         GBIFPGM 
07045 *                                                                 GBIFPGM 
07046 ******************************************************************GBIFPGM 
07047  4715-BLD-BC-SA-BP-MAX-POT.                                       GBIFPGM 
07048                                                                   GBIFPGM 
07049      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSDI'                     GBIFPGM 
07050         IF WS-PROCESS-CON                                         GBIFPGM 
07051            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07052            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-SA-BP-MAX-POT-IN   GBIFPGM 
07053         END-IF                                                    GBIFPGM 
07054      END-IF                                                       GBIFPGM 
07055      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSDO'                     GBIFPGM 
07056         IF WS-PROCESS-GRP                                         GBIFPGM 
07057            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07058            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-SA-BP-MAX-POT-OUT  GBIFPGM 
07059         END-IF                                                    GBIFPGM 
07060      END-IF                                                       GBIFPGM 
07061      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSDA'                     GBIFPGM 
07062         IF WS-PROCESS-CON                                         GBIFPGM 
07063            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07064            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-SA-BP-MAX-POT-OTH  GBIFPGM 
07065         END-IF                                                    GBIFPGM 
07066      END-IF.                                                      GBIFPGM 
07067                                                                   GBIFPGM 
07068  4715-EXIT.                                                       GBIFPGM 
07069      EXIT.                                                        GBIFPGM 
07070 /                                                                 GBIFPGM 
07071 ******************************************************************GBIFPGM 
07072 *                                                                 GBIFPGM 
07073 *    B/S SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM                   GBIFPGM 
07074 *                                                                 GBIFPGM 
07075 ******************************************************************GBIFPGM 
07076  4720-BLD-IP-BS-SA-BEN-PERD-MAX.                                  GBIFPGM 
07077                                                                   GBIFPGM 
07078      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSMI'                     GBIFPGM 
07079         IF WS-PROCESS-CON                                         GBIFPGM 
07080            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07081            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-SA-BP-MAX-IN    GBIFPGM 
07082         END-IF                                                    GBIFPGM 
07083      END-IF                                                       GBIFPGM 
07084      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSMO'                     GBIFPGM 
07085         IF WS-PROCESS-GRP                                         GBIFPGM 
07086            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07087            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-SA-BP-MAX-OUT   GBIFPGM 
07088         END-IF                                                    GBIFPGM 
07089      END-IF                                                       GBIFPGM 
07090      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSMA'                     GBIFPGM 
07091         IF WS-PROCESS-CON                                         GBIFPGM 
07092            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07093            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-SA-BP-MAX-OTH   GBIFPGM 
07094         END-IF                                                    GBIFPGM 
07095      END-IF.                                                      GBIFPGM 
07096                                                                   GBIFPGM 
07097  4720-EXIT.                                                       GBIFPGM 
07098      EXIT.                                                        GBIFPGM 
07099 /                                                                 GBIFPGM 
07100 *MQ 10/03                                                         GBIFPGM 
07101 ******************************************************************GBIFPGM 
07102 *                                                                 GBIFPGM 
07103 *    B/S SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM - ALL POT         GBIFPGM 
07104 *                                                                 GBIFPGM 
07105 ******************************************************************GBIFPGM 
07106  4725-BLD-BS-SA-BP-MAX-POT.                                       GBIFPGM 
07107                                                                   GBIFPGM 
07108      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSDI'                     GBIFPGM 
07109         IF WS-PROCESS-CON                                         GBIFPGM 
07110            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07111            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-SA-BP-MAX-POT-IN   GBIFPGM 
07112         END-IF                                                    GBIFPGM 
07113      END-IF                                                       GBIFPGM 
07114      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSDO'                     GBIFPGM 
07115         IF WS-PROCESS-GRP                                         GBIFPGM 
07116            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07117            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-SA-BP-MAX-POT-OUT  GBIFPGM 
07118         END-IF                                                    GBIFPGM 
07119      END-IF                                                       GBIFPGM 
07120      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSDA'                     GBIFPGM 
07121         IF WS-PROCESS-CON                                         GBIFPGM 
07122            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07123            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-SA-BP-MAX-POT-OTH  GBIFPGM 
07124         END-IF                                                    GBIFPGM 
07125      END-IF.                                                      GBIFPGM 
07126                                                                   GBIFPGM 
07127  4725-EXIT.                                                       GBIFPGM 
07128      EXIT.                                                        GBIFPGM 
07129 /                                                                 GBIFPGM 
07130 ******************************************************************GBIFPGM 
07131 *                                                                 GBIFPGM 
07132 *    B/C AND B/S SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM           GBIFPGM 
07133 *                                                                 GBIFPGM 
07134 ******************************************************************GBIFPGM 
07135  4730-BLD-IP-BCBS-SA-BP-MAX.                                      GBIFPGM 
07136                                                                   GBIFPGM 
07137      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASMI'                     GBIFPGM 
07138         IF WS-PROCESS-CON                                         GBIFPGM 
07139            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07140            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-SA-BP-MAX-IN  GBIFPGM 
07141         END-IF                                                    GBIFPGM 
07142      END-IF                                                       GBIFPGM 
07143      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASMO'                     GBIFPGM 
07144         IF WS-PROCESS-GRP                                         GBIFPGM 
07145            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07146            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-SA-BP-MAX-OUT GBIFPGM 
07147         END-IF                                                    GBIFPGM 
07148      END-IF                                                       GBIFPGM 
07149      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASMA'                     GBIFPGM 
07150         IF WS-PROCESS-CON                                         GBIFPGM 
07151            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07152            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-SA-BP-MAX-OTH GBIFPGM 
07153         END-IF                                                    GBIFPGM 
07154      END-IF.                                                      GBIFPGM 
07155                                                                   GBIFPGM 
07156  4730-EXIT.                                                       GBIFPGM 
07157      EXIT.                                                        GBIFPGM 
07158 /                                                                 GBIFPGM 
07159 *MQ 10/03                                                         GBIFPGM 
07160 ******************************************************************GBIFPGM 
07161 *                                                                 GBIFPGM 
07162 *    B/C AND B/S SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM - ALL POT GBIFPGM 
07163 *                                                                 GBIFPGM 
07164 ******************************************************************GBIFPGM 
07165  4735-BLD-BCBS-SA-BP-MAX-POT.                                     GBIFPGM 
07166                                                                   GBIFPGM 
07167      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASDI'                     GBIFPGM 
07168         IF WS-PROCESS-CON                                         GBIFPGM 
07169            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07170            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07171                               TO GCBH-BCBS-SA-BP-MAX-POT-IN       GBIFPGM 
07172         END-IF                                                    GBIFPGM 
07173      END-IF                                                       GBIFPGM 
07174      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASDO'                     GBIFPGM 
07175         IF WS-PROCESS-GRP                                         GBIFPGM 
07176            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07177            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07178                               TO GCBH-BCBS-SA-BP-MAX-POT-OUT      GBIFPGM 
07179         END-IF                                                    GBIFPGM 
07180      END-IF                                                       GBIFPGM 
07181      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASDA'                     GBIFPGM 
07182         IF WS-PROCESS-CON                                         GBIFPGM 
07183            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07184            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07185                               TO GCBH-BCBS-SA-BP-MAX-POT-OTH      GBIFPGM 
07186         END-IF                                                    GBIFPGM 
07187      END-IF.                                                      GBIFPGM 
07188                                                                   GBIFPGM 
07189  4735-EXIT.                                                       GBIFPGM 
07190      EXIT.                                                        GBIFPGM 
07191 /                                                                 GBIFPGM 
07192 ******************************************************************GBIFPGM 
07193 *                                                                 GBIFPGM 
07194 *    B/C SUBSTANCE ABUSE LIFETIME MAXIMUM                         GBIFPGM 
07195 *                                                                 GBIFPGM 
07196 ******************************************************************GBIFPGM 
07197  4740-BLD-IP-BC-SA-LIFE-MAX.                                      GBIFPGM 
07198                                                                   GBIFPGM 
07199      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSLI'                     GBIFPGM 
07200         IF WS-PROCESS-CON                                         GBIFPGM 
07201            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07202            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-SA-LIFE-MAX-IN  GBIFPGM 
07203         END-IF                                                    GBIFPGM 
07204      END-IF                                                       GBIFPGM 
07205      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSLO'                     GBIFPGM 
07206         IF WS-PROCESS-GRP                                         GBIFPGM 
07207            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07208            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-SA-LIFE-MAX-OUT GBIFPGM 
07209         END-IF                                                    GBIFPGM 
07210      END-IF                                                       GBIFPGM 
07211      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSLA'                     GBIFPGM 
07212         IF WS-PROCESS-CON                                         GBIFPGM 
07213            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07214            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BC-SA-LIFE-MAX-OTH GBIFPGM 
07215         END-IF                                                    GBIFPGM 
07216      END-IF.                                                      GBIFPGM 
07217                                                                   GBIFPGM 
07218  4740-EXIT.                                                       GBIFPGM 
07219      EXIT.                                                        GBIFPGM 
07220 /                                                                 GBIFPGM 
07221 *MQ 10/03                                                         GBIFPGM 
07222 ******************************************************************GBIFPGM 
07223 *                                                                 GBIFPGM 
07224 *    B/C SUBSTANCE ABUSE LIFETIME MAXIMUM - ALL POT               GBIFPGM 
07225 *                                                                 GBIFPGM 
07226 ******************************************************************GBIFPGM 
07227  4745-BLD-BC-SA-LIFE-MAX-POT.                                     GBIFPGM 
07228                                                                   GBIFPGM 
07229      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSFI'                     GBIFPGM 
07230         IF WS-PROCESS-CON                                         GBIFPGM 
07231            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07232            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-SA-LIFE-MAX-POT-IN GBIFPGM 
07233         END-IF                                                    GBIFPGM 
07234      END-IF                                                       GBIFPGM 
07235      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSFO'                     GBIFPGM 
07236         IF WS-PROCESS-GRP                                         GBIFPGM 
07237            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07238            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-SA-LIFE-MAX-POT-OUTGBIFPGM 
07239         END-IF                                                    GBIFPGM 
07240      END-IF                                                       GBIFPGM 
07241      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSFA'                     GBIFPGM 
07242         IF WS-PROCESS-CON                                         GBIFPGM 
07243            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07244            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BC-SA-LIFE-MAX-POT-OTHGBIFPGM 
07245         END-IF                                                    GBIFPGM 
07246      END-IF.                                                      GBIFPGM 
07247                                                                   GBIFPGM 
07248  4745-EXIT.                                                       GBIFPGM 
07249      EXIT.                                                        GBIFPGM 
07250 /                                                                 GBIFPGM 
07251 ******************************************************************GBIFPGM 
07252 *                                                                 GBIFPGM 
07253 *    B/S SUBSTANCE ABUSE LIFETIME MAXIMUM                         GBIFPGM 
07254 *                                                                 GBIFPGM 
07255 ******************************************************************GBIFPGM 
07256  4750-BLD-IP-BS-SA-LIFE-MAX.                                      GBIFPGM 
07257                                                                   GBIFPGM 
07258      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSLI'                     GBIFPGM 
07259         IF WS-PROCESS-CON                                         GBIFPGM 
07260            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07261            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-SA-LIFE-MAX-IN  GBIFPGM 
07262         END-IF                                                    GBIFPGM 
07263      END-IF                                                       GBIFPGM 
07264      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSLO'                     GBIFPGM 
07265         IF WS-PROCESS-GRP                                         GBIFPGM 
07266            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07267            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-SA-LIFE-MAX-OUT GBIFPGM 
07268         END-IF                                                    GBIFPGM 
07269      END-IF                                                       GBIFPGM 
07270      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSLA'                     GBIFPGM 
07271         IF WS-PROCESS-CON                                         GBIFPGM 
07272            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07273            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BS-SA-LIFE-MAX-OTH GBIFPGM 
07274         END-IF                                                    GBIFPGM 
07275      END-IF.                                                      GBIFPGM 
07276                                                                   GBIFPGM 
07277  4750-EXIT.                                                       GBIFPGM 
07278      EXIT.                                                        GBIFPGM 
07279 /                                                                 GBIFPGM 
07280 *MQ 10/03                                                         GBIFPGM 
07281 ******************************************************************GBIFPGM 
07282 *                                                                 GBIFPGM 
07283 *    B/S SUBSTANCE ABUSE LIFETIME MAXIMUM - ALL POT               GBIFPGM 
07284 *                                                                 GBIFPGM 
07285 ******************************************************************GBIFPGM 
07286  4755-BLD-BS-SA-LIFE-MAX-POT.                                     GBIFPGM 
07287                                                                   GBIFPGM 
07288      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSFI'                     GBIFPGM 
07289         IF WS-PROCESS-CON                                         GBIFPGM 
07290            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07291            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-SA-LIFE-MAX-POT-IN GBIFPGM 
07292         END-IF                                                    GBIFPGM 
07293      END-IF                                                       GBIFPGM 
07294      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSFO'                     GBIFPGM 
07295         IF WS-PROCESS-GRP                                         GBIFPGM 
07296            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07297            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-SA-LIFE-MAX-POT-OUTGBIFPGM 
07298         END-IF                                                    GBIFPGM 
07299      END-IF                                                       GBIFPGM 
07300      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSFA'                     GBIFPGM 
07301         IF WS-PROCESS-CON                                         GBIFPGM 
07302            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07303            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-BS-SA-LIFE-MAX-POT-OTHGBIFPGM 
07304         END-IF                                                    GBIFPGM 
07305      END-IF.                                                      GBIFPGM 
07306                                                                   GBIFPGM 
07307  4755-EXIT.                                                       GBIFPGM 
07308      EXIT.                                                        GBIFPGM 
07309 /                                                                 GBIFPGM 
07310 ******************************************************************GBIFPGM 
07311 *                                                                 GBIFPGM 
07312 *    B/C AND B/S SUBSTANCE ABUSE LIFETIME MAXIMUM                 GBIFPGM 
07313 *                                                                 GBIFPGM 
07314 ******************************************************************GBIFPGM 
07315  4760-BLD-IP-BCBS-SA-LIFE-MAX.                                    GBIFPGM 
07316                                                                   GBIFPGM 
07317      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASLI'                     GBIFPGM 
07318         IF WS-PROCESS-CON                                         GBIFPGM 
07319            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07320            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-IP-BCBS-SA-LIFE-MAX-INGBIFPGM 
07321         END-IF                                                    GBIFPGM 
07322      END-IF                                                       GBIFPGM 
07323      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASLO'                     GBIFPGM 
07324         IF WS-PROCESS-GRP                                         GBIFPGM 
07325            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07326            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07327                               TO GCBH-IP-BCBS-SA-LIFE-MAX-OUT     GBIFPGM 
07328         END-IF                                                    GBIFPGM 
07329      END-IF                                                       GBIFPGM 
07330      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASLA'                     GBIFPGM 
07331         IF WS-PROCESS-CON                                         GBIFPGM 
07332            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07333            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07334                               TO GCBH-IP-BCBS-SA-LIFE-MAX-OTH     GBIFPGM 
07335         END-IF                                                    GBIFPGM 
07336      END-IF.                                                      GBIFPGM 
07337                                                                   GBIFPGM 
07338  4760-EXIT.                                                       GBIFPGM 
07339      EXIT.                                                        GBIFPGM 
07340 /                                                                 GBIFPGM 
07341 *MQ 10/03                                                         GBIFPGM 
07342 ******************************************************************GBIFPGM 
07343 *                                                                 GBIFPGM 
07344 *    SUBSTANCE ABUSE LIFETIME CONFINEMENT MAXIMUM                 GBIFPGM 
07345 *                                                                 GBIFPGM 
07346 ******************************************************************GBIFPGM 
07347  4762-BLD-SA-LIFE-CONF-MAX.                                       GBIFPGM 
07348                                                                   GBIFPGM 
09155      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SALA'                     GBIFPGM 
07350         IF WS-PROCESS-CON                                         GBIFPGM 
07351            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
09158            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SA-LIFE-CONF-MAX-IN   GBIFPGM 
07354         END-IF                                                    GBIFPGM 
07355      END-IF                                                       GBIFPGM 
09161      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SALO'                     GBIFPGM 
07357         IF WS-PROCESS-GRP                                         GBIFPGM 
07358            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
09164            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SA-LIFE-CONF-MAX-OUT  GBIFPGM 
07361         END-IF                                                    GBIFPGM 
09166      END-IF                                                       GBIFPGM 
09167      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SALI'                     GBIFPGM 
09168         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
09169         MOVE WS-TEMP-VALUE-LIMIT TO GCBH-SA-LIFE-CONF-MAX-OTH     GBIFPGM 
07362      END-IF.                                                      GBIFPGM 
07363                                                                   GBIFPGM 
07364  4762-EXIT.                                                       GBIFPGM 
07365      EXIT.                                                        GBIFPGM 
07366 /                                                                 GBIFPGM 
07367 *MQ 10/03                                                         GBIFPGM 
07368 ******************************************************************GBIFPGM 
07369 *                                                                 GBIFPGM 
07370 *    B/C AND B/S SUBSTANCE ABUSE LIFETIME MAXIMUM - ALL POT       GBIFPGM 
07371 *                                                                 GBIFPGM 
07372 ******************************************************************GBIFPGM 
07373  4765-BLD-BCBS-SA-LIFE-MAX-POT.                                   GBIFPGM 
07374                                                                   GBIFPGM 
07375      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASFI'                     GBIFPGM 
07376         IF WS-PROCESS-CON                                         GBIFPGM 
07377            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07378            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07379                               TO GCBH-BCBS-SA-LIFE-MAX-POT-IN     GBIFPGM 
07380         END-IF                                                    GBIFPGM 
07381      END-IF                                                       GBIFPGM 
07382      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASFO'                     GBIFPGM 
07383         IF WS-PROCESS-GRP                                         GBIFPGM 
07384            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07385            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07386                               TO GCBH-BCBS-SA-LIFE-MAX-POT-OUT    GBIFPGM 
07387         END-IF                                                    GBIFPGM 
07388      END-IF                                                       GBIFPGM 
07389      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASFA'                     GBIFPGM 
07390         IF WS-PROCESS-CON                                         GBIFPGM 
07391            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07392            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07393                               TO GCBH-BCBS-SA-LIFE-MAX-POT-OTH    GBIFPGM 
07394         END-IF                                                    GBIFPGM 
07395      END-IF.                                                      GBIFPGM 
07396                                                                   GBIFPGM 
07397  4765-EXIT.                                                       GBIFPGM 
07398      EXIT.                                                        GBIFPGM 
07399 /                                                                 GBIFPGM 
07400 ******************************************************************GBIFPGM 
07401 *                                                                 GBIFPGM 
07402 *    B/C MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL                   GBIFPGM 
07403 *                                                                 GBIFPGM 
07404 ******************************************************************GBIFPGM 
07405  4770-BLD-OP-BC-MSA-PMT-LVL.                                      GBIFPGM 
07406                                                                   GBIFPGM 
07407      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBPI'                    GBIFPGM 
07408         IF WS-PROCESS-CON                                         GBIFPGM 
07409            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07410                                       TO WS-PERCENT-VAL           GBIFPGM 
07411            MOVE WS-PERCENT         TO GCBH-OP-BC-MSA-PMT-LVL-IN   GBIFPGM 
07412         END-IF                                                    GBIFPGM 
07413      END-IF                                                       GBIFPGM 
07414      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBPO'                    GBIFPGM 
07415         IF WS-PROCESS-GRP                                         GBIFPGM 
07416            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07417                                       TO WS-PERCENT-VAL           GBIFPGM 
07418            MOVE WS-PERCENT         TO GCBH-OP-BC-MSA-PMT-LVL-OUT  GBIFPGM 
07419         END-IF                                                    GBIFPGM 
07420      END-IF                                                       GBIFPGM 
07421      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBPA'                    GBIFPGM 
07422         IF WS-PROCESS-CON                                         GBIFPGM 
07423            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07424                                          TO WS-PERCENT-VAL        GBIFPGM 
07425            MOVE WS-PERCENT         TO GCBH-OP-BC-MSA-PMT-LVL-OTH  GBIFPGM 
07426         END-IF                                                    GBIFPGM 
07427      END-IF.                                                      GBIFPGM 
07428                                                                   GBIFPGM 
07429  4770-EXIT.                                                       GBIFPGM 
07430      EXIT.                                                        GBIFPGM 
07431 /                                                                 GBIFPGM 
07432 *MQ 10/03                                                         GBIFPGM 
07433 ******************************************************************GBIFPGM 
07434 *                                                                 GBIFPGM 
07435 *    B/C MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL - ALL POT         GBIFPGM 
07436 *                                                                 GBIFPGM 
07437 ******************************************************************GBIFPGM 
07438  4775-BLD-BC-MSA-PMT-LVL-POT.                                     GBIFPGM 
07439                                                                   GBIFPGM 
07440      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBAI'                    GBIFPGM 
07441         IF WS-PROCESS-CON                                         GBIFPGM 
07442            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07443                                       TO WS-PERCENT-VAL           GBIFPGM 
07444            MOVE WS-PERCENT                                        GBIFPGM 
07445                              TO GCBH-BC-MSA-PMT-LVL-POT-IN        GBIFPGM 
07446         END-IF                                                    GBIFPGM 
07447      END-IF                                                       GBIFPGM 
07448      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBAO'                    GBIFPGM 
07449         IF WS-PROCESS-GRP                                         GBIFPGM 
07450            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07451                                       TO WS-PERCENT-VAL           GBIFPGM 
07452            MOVE WS-PERCENT                                        GBIFPGM 
07453                              TO GCBH-BC-MSA-PMT-LVL-POT-OUT       GBIFPGM 
07454         END-IF                                                    GBIFPGM 
07455      END-IF                                                       GBIFPGM 
07456      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CBAA'                    GBIFPGM 
07457         IF WS-PROCESS-CON                                         GBIFPGM 
07458            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07459                                          TO WS-PERCENT-VAL        GBIFPGM 
07460            MOVE WS-PERCENT                                        GBIFPGM 
07461                              TO GCBH-BC-MSA-PMT-LVL-POT-OTH       GBIFPGM 
07462         END-IF                                                    GBIFPGM 
07463      END-IF.                                                      GBIFPGM 
07464                                                                   GBIFPGM 
07465  4775-EXIT.                                                       GBIFPGM 
07466      EXIT.                                                        GBIFPGM 
07467 /                                                                 GBIFPGM 
07468 ******************************************************************GBIFPGM 
07469 *                                                                 GBIFPGM 
07470 *    B/S MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL                   GBIFPGM 
07471 *                                                                 GBIFPGM 
07472 ******************************************************************GBIFPGM 
07473  4780-BLD-OP-BS-MSA-PMT-LVL.                                      GBIFPGM 
07474                                                                   GBIFPGM 
07475      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBPI'                    GBIFPGM 
07476         IF WS-PROCESS-CON                                         GBIFPGM 
07477            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07478                                       TO WS-PERCENT-VAL           GBIFPGM 
07479            MOVE WS-PERCENT         TO GCBH-OP-BS-MSA-PMT-LVL-IN   GBIFPGM 
07480         END-IF                                                    GBIFPGM 
07481      END-IF                                                       GBIFPGM 
07482      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBPO'                    GBIFPGM 
07483         IF WS-PROCESS-GRP                                         GBIFPGM 
07484            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07485                                       TO WS-PERCENT-VAL           GBIFPGM 
07486            MOVE WS-PERCENT         TO GCBH-OP-BS-MSA-PMT-LVL-OUT  GBIFPGM 
07487         END-IF                                                    GBIFPGM 
07488      END-IF                                                       GBIFPGM 
07489      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBPA'                    GBIFPGM 
07490         IF WS-PROCESS-CON                                         GBIFPGM 
07491            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07492                                          TO WS-PERCENT-VAL        GBIFPGM 
07493            MOVE WS-PERCENT         TO GCBH-OP-BS-MSA-PMT-LVL-OTH  GBIFPGM 
07494         END-IF                                                    GBIFPGM 
07495      END-IF.                                                      GBIFPGM 
07496                                                                   GBIFPGM 
07497  4780-EXIT.                                                       GBIFPGM 
07498      EXIT.                                                        GBIFPGM 
07499 /                                                                 GBIFPGM 
07500 *MQ 10/03                                                         GBIFPGM 
07501 ******************************************************************GBIFPGM 
07502 *                                                                 GBIFPGM 
07503 *    B/S MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL - ALL POT         GBIFPGM 
07504 *                                                                 GBIFPGM 
07505 ******************************************************************GBIFPGM 
07506  4785-BLD-BS-MSA-PMT-LVL-POT.                                     GBIFPGM 
07507                                                                   GBIFPGM 
07508      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBAI'                    GBIFPGM 
07509         IF WS-PROCESS-CON                                         GBIFPGM 
07510            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07511                             TO WS-PERCENT-VAL                     GBIFPGM 
07512            MOVE WS-PERCENT                                        GBIFPGM 
07513                             TO GCBH-BS-MSA-PMT-LVL-POT-IN         GBIFPGM 
07514         END-IF                                                    GBIFPGM 
07515      END-IF                                                       GBIFPGM 
07516      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBAO'                    GBIFPGM 
07517         IF WS-PROCESS-GRP                                         GBIFPGM 
07518            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07519                                       TO WS-PERCENT-VAL           GBIFPGM 
07520            MOVE WS-PERCENT                                        GBIFPGM 
07521                             TO GCBH-BS-MSA-PMT-LVL-POT-OUT        GBIFPGM 
07522         END-IF                                                    GBIFPGM 
07523      END-IF                                                       GBIFPGM 
07524      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SBAA'                    GBIFPGM 
07525         IF WS-PROCESS-CON                                         GBIFPGM 
07526            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07527                                          TO WS-PERCENT-VAL        GBIFPGM 
07528            MOVE WS-PERCENT                                        GBIFPGM 
07529                             TO GCBH-BS-MSA-PMT-LVL-POT-OTH        GBIFPGM 
07530         END-IF                                                    GBIFPGM 
07531      END-IF.                                                      GBIFPGM 
07532                                                                   GBIFPGM 
07533  4785-EXIT.                                                       GBIFPGM 
07534      EXIT.                                                        GBIFPGM 
07535 /                                                                 GBIFPGM 
07536 ******************************************************************GBIFPGM 
07537 *                                                                 GBIFPGM 
07538 *    B/C AND B/S MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL           GBIFPGM 
07539 *                                                                 GBIFPGM 
07540 ******************************************************************GBIFPGM 
07541  4790-BLD-OP-BCBS-MSA-PMT-LVL.                                    GBIFPGM 
07542                                                                   GBIFPGM 
07543      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABPI'                    GBIFPGM 
07544         IF WS-PROCESS-CON                                         GBIFPGM 
07545            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07546                                       TO WS-PERCENT-VAL           GBIFPGM 
07547            MOVE WS-PERCENT       TO GCBH-OP-BCBS-MSA-PMT-LVL-IN   GBIFPGM 
07548         END-IF                                                    GBIFPGM 
07549      END-IF                                                       GBIFPGM 
07550      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABPO'                    GBIFPGM 
07551         IF WS-PROCESS-GRP                                         GBIFPGM 
07552            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07553                                       TO WS-PERCENT-VAL           GBIFPGM 
07554            MOVE WS-PERCENT       TO GCBH-OP-BCBS-MSA-PMT-LVL-OUT  GBIFPGM 
07555         END-IF                                                    GBIFPGM 
07556      END-IF                                                       GBIFPGM 
07557      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABPA'                    GBIFPGM 
07558         IF WS-PROCESS-CON                                         GBIFPGM 
07559            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07560                                          TO WS-PERCENT-VAL        GBIFPGM 
07561            MOVE WS-PERCENT       TO GCBH-OP-BCBS-MSA-PMT-LVL-OTH  GBIFPGM 
07562         END-IF                                                    GBIFPGM 
07563      END-IF.                                                      GBIFPGM 
07564                                                                   GBIFPGM 
07565  4790-EXIT.                                                       GBIFPGM 
07566      EXIT.                                                        GBIFPGM 
07567 /                                                                 GBIFPGM 
07568 *MQ 10/03                                                         GBIFPGM 
07569 ******************************************************************GBIFPGM 
07570 *                                                                 GBIFPGM 
07571 *    B/C AND B/S MENTAL / SUBSTANCE ABUSE PAYMENT LEVEL - ALL POT GBIFPGM 
07572 *                                                                 GBIFPGM 
07573 ******************************************************************GBIFPGM 
07574  4795-BLD-BCBS-MSA-PMT-LVL-POT.                                   GBIFPGM 
07575                                                                   GBIFPGM 
07576      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABAI'                    GBIFPGM 
07577         IF WS-PROCESS-CON                                         GBIFPGM 
07578            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07579                                       TO WS-PERCENT-VAL           GBIFPGM 
07580            MOVE WS-PERCENT                                        GBIFPGM 
07581                           TO GCBH-BCBS-MSA-PMT-LVL-POT-IN         GBIFPGM 
07582         END-IF                                                    GBIFPGM 
07583      END-IF                                                       GBIFPGM 
07584      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABAO'                    GBIFPGM 
07585         IF WS-PROCESS-GRP                                         GBIFPGM 
07586            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07587                                       TO WS-PERCENT-VAL           GBIFPGM 
07588            MOVE WS-PERCENT                                        GBIFPGM 
07589                           TO GCBH-BCBS-MSA-PMT-LVL-POT-OUT        GBIFPGM 
07590         END-IF                                                    GBIFPGM 
07591      END-IF                                                       GBIFPGM 
07592      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ABAA'                    GBIFPGM 
07593         IF WS-PROCESS-CON                                         GBIFPGM 
07594            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07595                                          TO WS-PERCENT-VAL        GBIFPGM 
07596            MOVE WS-PERCENT                                        GBIFPGM 
07597                           TO GCBH-BCBS-MSA-PMT-LVL-POT-OTH        GBIFPGM 
07598         END-IF                                                    GBIFPGM 
07599      END-IF.                                                      GBIFPGM 
07600                                                                   GBIFPGM 
07601  4795-EXIT.                                                       GBIFPGM 
07602      EXIT.                                                        GBIFPGM 
07603 /                                                                 GBIFPGM 
07604 ******************************************************************GBIFPGM 
07605 *                                                                 GBIFPGM 
07606 *    B/C MENTAL / SA BENEFIT PERIOD MAXIMUM                       GBIFPGM 
07607 *                                                                 GBIFPGM 
07608 ******************************************************************GBIFPGM 
07609  4800-BLD-OP-BC-MSA-BP-MAX.                                       GBIFPGM 
07610                                                                   GBIFPGM 
07611      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBBI'                     GBIFPGM 
07612         IF WS-PROCESS-CON                                         GBIFPGM 
07613            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07614            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MSA-BP-MAX-IN   GBIFPGM 
07615         END-IF                                                    GBIFPGM 
07616      END-IF                                                       GBIFPGM 
07617      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBBO'                     GBIFPGM 
07618         IF WS-PROCESS-GRP                                         GBIFPGM 
07619            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07620            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MSA-BP-MAX-OUT  GBIFPGM 
07621         END-IF                                                    GBIFPGM 
07622      END-IF                                                       GBIFPGM 
07623      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBBA'                     GBIFPGM 
07624         IF WS-PROCESS-CON                                         GBIFPGM 
07625            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07626            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MSA-BP-MAX-OTH  GBIFPGM 
07627         END-IF                                                    GBIFPGM 
07628      END-IF.                                                      GBIFPGM 
07629                                                                   GBIFPGM 
07630  4800-EXIT.                                                       GBIFPGM 
07631      EXIT.                                                        GBIFPGM 
07632 /                                                                 GBIFPGM 
07633 ******************************************************************GBIFPGM 
07634 *                                                                 GBIFPGM 
07635 *    B/S MENTAL / SA BENEFIT PERIOD MAXIMUM                       GBIFPGM 
07636 *                                                                 GBIFPGM 
07637 ******************************************************************GBIFPGM 
07638  4810-BLD-OP-BS-MSA-BP-MAX.                                       GBIFPGM 
07639                                                                   GBIFPGM 
07640      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBBI'                     GBIFPGM 
07641         IF WS-PROCESS-CON                                         GBIFPGM 
07642            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07643            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MSA-BP-MAX-IN   GBIFPGM 
07644         END-IF                                                    GBIFPGM 
07645      END-IF                                                       GBIFPGM 
07646      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBBO'                     GBIFPGM 
07647         IF WS-PROCESS-GRP                                         GBIFPGM 
07648            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07649            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MSA-BP-MAX-OUT  GBIFPGM 
07650         END-IF                                                    GBIFPGM 
07651      END-IF                                                       GBIFPGM 
07652      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBBA'                     GBIFPGM 
07653         IF WS-PROCESS-CON                                         GBIFPGM 
07654            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07655            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MSA-BP-MAX-OTH  GBIFPGM 
07656         END-IF                                                    GBIFPGM 
07657      END-IF.                                                      GBIFPGM 
07658                                                                   GBIFPGM 
07659  4810-EXIT.                                                       GBIFPGM 
07660      EXIT.                                                        GBIFPGM 
07661 /                                                                 GBIFPGM 
07662 ******************************************************************GBIFPGM 
07663 *                                                                 GBIFPGM 
07664 *    B/C AND B/S MENTAL SA BENEFIT PERIOD MAXIMUM                 GBIFPGM 
07665 *                                                                 GBIFPGM 
07666 ******************************************************************GBIFPGM 
07667  4820-BLD-OP-BCBS-MSA-BP-MAX.                                     GBIFPGM 
07668                                                                   GBIFPGM 
07669      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABBI'                     GBIFPGM 
07670         IF WS-PROCESS-CON                                         GBIFPGM 
07671            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07672            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-MSA-BP-MAX-IN GBIFPGM 
07673         END-IF                                                    GBIFPGM 
07674      END-IF                                                       GBIFPGM 
07675      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABBO'                     GBIFPGM 
07676         IF WS-PROCESS-GRP                                         GBIFPGM 
07677            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07678            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-MSA-BP-MAX-OUTGBIFPGM 
07679         END-IF                                                    GBIFPGM 
07680      END-IF                                                       GBIFPGM 
07681      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABBA'                     GBIFPGM 
07682         IF WS-PROCESS-CON                                         GBIFPGM 
07683            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07684            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07685                               TO GCBH-OP-BCBS-MSA-BP-MAX-OTH      GBIFPGM 
07686         END-IF                                                    GBIFPGM 
07687      END-IF.                                                      GBIFPGM 
07688                                                                   GBIFPGM 
07689  4820-EXIT.                                                       GBIFPGM 
07690      EXIT.                                                        GBIFPGM 
07691 /                                                                 GBIFPGM 
07692 *MQ 10/03                                                         GBIFPGM 
07693 ******************************************************************GBIFPGM 
07694 *                                                                 GBIFPGM 
07695 *    B/C AND B/S OUTPATIENT MENTAL SA BENEFIT PERIOD MAX-CHILD    GBIFPGM 
07696 *                                                                 GBIFPGM 
07697 ******************************************************************GBIFPGM 
07698  4822-BLD-BCBS-OP-MSA-BP-MAX-CH.                                  GBIFPGM 
07699                                                                   GBIFPGM 
07700      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACMI'                     GBIFPGM 
07701         IF WS-PROCESS-CON                                         GBIFPGM 
07702            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07703            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07704                              TO GCBH-BCBS-OP-MSA-BP-MAX-CH-IN     GBIFPGM 
07705         END-IF                                                    GBIFPGM 
07706      END-IF                                                       GBIFPGM 
07707      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACMO'                     GBIFPGM 
07708         IF WS-PROCESS-GRP                                         GBIFPGM 
07709            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07710            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07711                              TO GCBH-BCBS-OP-MSA-BP-MAX-CH-OUT    GBIFPGM 
07712         END-IF                                                    GBIFPGM 
07713      END-IF                                                       GBIFPGM 
07714      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACMA'                     GBIFPGM 
07715         IF WS-PROCESS-CON                                         GBIFPGM 
07716            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07717            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07718                              TO GCBH-BCBS-OP-MSA-BP-MAX-CH-OTH    GBIFPGM 
07719         END-IF                                                    GBIFPGM 
07720      END-IF.                                                      GBIFPGM 
07721                                                                   GBIFPGM 
07722  4822-EXIT.                                                       GBIFPGM 
07723      EXIT.                                                        GBIFPGM 
07724 /                                                                 GBIFPGM 
07725 *MQ 10/03                                                         GBIFPGM 
07726 ******************************************************************GBIFPGM 
07727 *                                                                 GBIFPGM 
07728 *    B/C AND B/S OUTPATIENT MENTAL SA BENEFIT PERIOD MAX-ADULT    GBIFPGM 
07729 *                                                                 GBIFPGM 
07730 ******************************************************************GBIFPGM 
07731  4825-BLD-BCBS-OP-MSA-BP-MAX-AD.                                  GBIFPGM 
07732                                                                   GBIFPGM 
07733      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AAMI'                     GBIFPGM 
07734         IF WS-PROCESS-CON                                         GBIFPGM 
07735            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07736            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07737                              TO GCBH-BCBS-OP-MSA-BP-MAX-AD-IN     GBIFPGM 
07738         END-IF                                                    GBIFPGM 
07739      END-IF                                                       GBIFPGM 
07740      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AAMO'                     GBIFPGM 
07741         IF WS-PROCESS-GRP                                         GBIFPGM 
07742            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07743            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07744                              TO GCBH-BCBS-OP-MSA-BP-MAX-AD-OUT    GBIFPGM 
07745         END-IF                                                    GBIFPGM 
07746      END-IF                                                       GBIFPGM 
07747      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AAMA'                     GBIFPGM 
07748         IF WS-PROCESS-CON                                         GBIFPGM 
07749            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07750            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07751                              TO GCBH-BCBS-OP-MSA-BP-MAX-AD-OTH    GBIFPGM 
07752         END-IF                                                    GBIFPGM 
07753      END-IF.                                                      GBIFPGM 
07754                                                                   GBIFPGM 
07755  4825-EXIT.                                                       GBIFPGM 
07756      EXIT.                                                        GBIFPGM 
07757 /                                                                 GBIFPGM 
07758 ******************************************************************GBIFPGM 
07759 *                                                                 GBIFPGM 
07760 *    B/C MENTAL SA LIFETIME MAXIMUM                               GBIFPGM 
07761 *                                                                 GBIFPGM 
07762 ******************************************************************GBIFPGM 
07763  4830-BLD-OP-BC-MSA-LIFE-MAX.                                     GBIFPGM 
07764                                                                   GBIFPGM 
07765      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBOI'                     GBIFPGM 
07766         IF WS-PROCESS-CON                                         GBIFPGM 
07767            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07768            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MSA-LIFE-MAX-IN GBIFPGM 
07769         END-IF                                                    GBIFPGM 
07770      END-IF                                                       GBIFPGM 
07771      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBOO'                     GBIFPGM 
07772         IF WS-PROCESS-GRP                                         GBIFPGM 
07773            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07774            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MSA-LIFE-MAX-OUTGBIFPGM 
07775         END-IF                                                    GBIFPGM 
07776      END-IF                                                       GBIFPGM 
07777      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CBOA'                     GBIFPGM 
07778         IF WS-PROCESS-CON                                         GBIFPGM 
07779            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07780            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07781                               TO GCBH-OP-BC-MSA-LIFE-MAX-OTH      GBIFPGM 
07782         END-IF                                                    GBIFPGM 
07783      END-IF.                                                      GBIFPGM 
07784                                                                   GBIFPGM 
07785  4830-EXIT.                                                       GBIFPGM 
07786      EXIT.                                                        GBIFPGM 
07787 /                                                                 GBIFPGM 
07788 ******************************************************************GBIFPGM 
07789 *                                                                 GBIFPGM 
07790 *    B/S MENTAL SA LIFETIME MAXIMUM                               GBIFPGM 
07791 *                                                                 GBIFPGM 
07792 ******************************************************************GBIFPGM 
07793  4840-BLD-OP-BS-MSA-LIFE-MAX.                                     GBIFPGM 
07794                                                                   GBIFPGM 
07795      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBOI'                     GBIFPGM 
07796         IF WS-PROCESS-CON                                         GBIFPGM 
07797            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07798            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MSA-LIFE-MAX-IN GBIFPGM 
07799         END-IF                                                    GBIFPGM 
07800      END-IF                                                       GBIFPGM 
07801      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBOO'                     GBIFPGM 
07802         IF WS-PROCESS-GRP                                         GBIFPGM 
07803            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07804            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MSA-LIFE-MAX-OUTGBIFPGM 
07805         END-IF                                                    GBIFPGM 
07806      END-IF                                                       GBIFPGM 
07807      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SBOA'                     GBIFPGM 
07808         IF WS-PROCESS-CON                                         GBIFPGM 
07809            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07810            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07811                               TO GCBH-OP-BS-MSA-LIFE-MAX-OTH      GBIFPGM 
07812         END-IF                                                    GBIFPGM 
07813      END-IF.                                                      GBIFPGM 
07814                                                                   GBIFPGM 
07815  4840-EXIT.                                                       GBIFPGM 
07816      EXIT.                                                        GBIFPGM 
07817 /                                                                 GBIFPGM 
07818 ******************************************************************GBIFPGM 
07819 *                                                                 GBIFPGM 
07820 *    B/C AND B/S MENTAL SA LIFETIME MAXIMUM                       GBIFPGM 
07821 *                                                                 GBIFPGM 
07822 ******************************************************************GBIFPGM 
07823  4850-BLD-OP-BCBS-MSA-LIFE-MAX.                                   GBIFPGM 
07824                                                                   GBIFPGM 
07825      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABOI'                     GBIFPGM 
07826         IF WS-PROCESS-CON                                         GBIFPGM 
07827            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07828            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07829                               TO GCBH-OP-BCBS-MSA-LIFE-MAX-IN     GBIFPGM 
07830         END-IF                                                    GBIFPGM 
07831      END-IF                                                       GBIFPGM 
07832      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABOO'                     GBIFPGM 
07833         IF WS-PROCESS-GRP                                         GBIFPGM 
07834            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07835            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07836                               TO GCBH-OP-BCBS-MSA-LIFE-MAX-OUT    GBIFPGM 
07837         END-IF                                                    GBIFPGM 
07838      END-IF                                                       GBIFPGM 
07839      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ABOA'                     GBIFPGM 
07840         IF WS-PROCESS-CON                                         GBIFPGM 
07841            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
07842            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
07843                               TO GCBH-OP-BCBS-MSA-LIFE-MAX-OTH    GBIFPGM 
07844         END-IF                                                    GBIFPGM 
07845      END-IF.                                                      GBIFPGM 
07846                                                                   GBIFPGM 
07847  4850-EXIT.                                                       GBIFPGM 
07848      EXIT.                                                        GBIFPGM 
07849 /                                                                 GBIFPGM 
07850 ******************************************************************GBIFPGM 
07851 *                                                                 GBIFPGM 
07852 *    B/C MENTAL HEALTH PAYMENT LEVEL                              GBIFPGM 
07853 *                                                                 GBIFPGM 
07854 ******************************************************************GBIFPGM 
07855  4860-BLD-OP-BC-MH-PMT-LVL.                                       GBIFPGM 
07856                                                                   GBIFPGM 
07857      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMCI'                    GBIFPGM 
07858         IF WS-PROCESS-CON                                         GBIFPGM 
07859            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07860                                       TO WS-PERCENT-VAL           GBIFPGM 
07861            MOVE WS-PERCENT       TO GCBH-OP-BC-MH-PMT-LVL-IN      GBIFPGM 
07862         END-IF                                                    GBIFPGM 
07863      END-IF                                                       GBIFPGM 
07864      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMCO'                    GBIFPGM 
07865         IF WS-PROCESS-GRP                                         GBIFPGM 
07866            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07867                                       TO WS-PERCENT-VAL           GBIFPGM 
07868            MOVE WS-PERCENT       TO GCBH-OP-BC-MH-PMT-LVL-OUT     GBIFPGM 
07869         END-IF                                                    GBIFPGM 
07870      END-IF                                                       GBIFPGM 
07871      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMCA'                    GBIFPGM 
07872         IF WS-PROCESS-CON                                         GBIFPGM 
07873            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07874                                          TO WS-PERCENT-VAL        GBIFPGM 
07875            MOVE WS-PERCENT       TO GCBH-OP-BC-MH-PMT-LVL-OTH     GBIFPGM 
07876         END-IF                                                    GBIFPGM 
07877      END-IF.                                                      GBIFPGM 
07878                                                                   GBIFPGM 
07879  4860-EXIT.                                                       GBIFPGM 
07880      EXIT.                                                        GBIFPGM 
07881 /                                                                 GBIFPGM 
07882 *MQ 10/03                                                         GBIFPGM 
07883 ******************************************************************GBIFPGM 
07884 *                                                                 GBIFPGM 
07885 *    B/C MENTAL HEALTH PAYMENT LEVEL - ALL POT                    GBIFPGM 
07886 *                                                                 GBIFPGM 
07887 ******************************************************************GBIFPGM 
07888  4865-BLD-BC-MH-PMT-LVL-POT.                                      GBIFPGM 
07889                                                                   GBIFPGM 
07890      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMAI'                    GBIFPGM 
07891         IF WS-PROCESS-CON                                         GBIFPGM 
07892            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07893                           TO WS-PERCENT-VAL                       GBIFPGM 
07894            MOVE WS-PERCENT                                        GBIFPGM 
07895                           TO GCBH-BC-MH-PMT-LVL-POT-IN            GBIFPGM 
07896         END-IF                                                    GBIFPGM 
07897      END-IF                                                       GBIFPGM 
07898      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMAO'                    GBIFPGM 
07899         IF WS-PROCESS-GRP                                         GBIFPGM 
07900            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07901                           TO WS-PERCENT-VAL                       GBIFPGM 
07902            MOVE WS-PERCENT                                        GBIFPGM 
07903                           TO GCBH-BC-MH-PMT-LVL-POT-OUT           GBIFPGM 
07904         END-IF                                                    GBIFPGM 
07905      END-IF                                                       GBIFPGM 
07906      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CMAA'                    GBIFPGM 
07907         IF WS-PROCESS-CON                                         GBIFPGM 
07908            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07909                           TO WS-PERCENT-VAL                       GBIFPGM 
07910            MOVE WS-PERCENT                                        GBIFPGM 
07911                           TO GCBH-BC-MH-PMT-LVL-POT-OTH           GBIFPGM 
07912         END-IF                                                    GBIFPGM 
07913      END-IF.                                                      GBIFPGM 
07914                                                                   GBIFPGM 
07915  4865-EXIT.                                                       GBIFPGM 
07916      EXIT.                                                        GBIFPGM 
07917 /                                                                 GBIFPGM 
07918 ******************************************************************GBIFPGM 
07919 *                                                                 GBIFPGM 
07920 *    B/S MENTAL HEALTH PAYMENT LEVEL                              GBIFPGM 
07921 *                                                                 GBIFPGM 
07922 ******************************************************************GBIFPGM 
07923  4870-BLD-OP-BS-MH-PMT-LVL.                                       GBIFPGM 
07924                                                                   GBIFPGM 
07925      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMCI'                    GBIFPGM 
07926         IF WS-PROCESS-CON                                         GBIFPGM 
07927            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07928                                       TO WS-PERCENT-VAL           GBIFPGM 
07929            MOVE WS-PERCENT            TO GCBH-OP-BS-MH-PMT-LVL-IN GBIFPGM 
07930         END-IF                                                    GBIFPGM 
07931      END-IF                                                       GBIFPGM 
07932      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMCO'                    GBIFPGM 
07933         IF WS-PROCESS-GRP                                         GBIFPGM 
07934            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07935                                       TO WS-PERCENT-VAL           GBIFPGM 
07936            MOVE WS-PERCENT            TO GCBH-OP-BS-MH-PMT-LVL-OUTGBIFPGM 
07937         END-IF                                                    GBIFPGM 
07938      END-IF                                                       GBIFPGM 
07939      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMCA'                    GBIFPGM 
07940         IF WS-PROCESS-CON                                         GBIFPGM 
07941            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07942                                          TO WS-PERCENT-VAL        GBIFPGM 
07943            MOVE WS-PERCENT       TO GCBH-OP-BS-MH-PMT-LVL-OTH     GBIFPGM 
07944         END-IF                                                    GBIFPGM 
07945      END-IF.                                                      GBIFPGM 
07946                                                                   GBIFPGM 
07947  4870-EXIT.                                                       GBIFPGM 
07948      EXIT.                                                        GBIFPGM 
07949 /                                                                 GBIFPGM 
07950 *MQ 10/03                                                         GBIFPGM 
07951 ******************************************************************GBIFPGM 
07952 *                                                                 GBIFPGM 
07953 *    B/S MENTAL HEALTH PAYMENT LEVEL - ALL POT                    GBIFPGM 
07954 *                                                                 GBIFPGM 
07955 ******************************************************************GBIFPGM 
07956  4875-BLD-BS-MH-PMT-LVL-POT.                                      GBIFPGM 
07957                                                                   GBIFPGM 
07958      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMAI'                    GBIFPGM 
07959         IF WS-PROCESS-CON                                         GBIFPGM 
07960            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07961                              TO WS-PERCENT-VAL                    GBIFPGM 
07962            MOVE WS-PERCENT                                        GBIFPGM 
07963                              TO GCBH-BS-MH-PMT-LVL-POT-IN         GBIFPGM 
07964         END-IF                                                    GBIFPGM 
07965      END-IF                                                       GBIFPGM 
07966      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMAO'                    GBIFPGM 
07967         IF WS-PROCESS-GRP                                         GBIFPGM 
07968            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07969                                       TO WS-PERCENT-VAL           GBIFPGM 
07970            MOVE WS-PERCENT                                        GBIFPGM 
07971                              TO GCBH-BS-MH-PMT-LVL-POT-OUT        GBIFPGM 
07972         END-IF                                                    GBIFPGM 
07973      END-IF                                                       GBIFPGM 
07974      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SMAA'                    GBIFPGM 
07975         IF WS-PROCESS-CON                                         GBIFPGM 
07976            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07977                                          TO WS-PERCENT-VAL        GBIFPGM 
07978            MOVE WS-PERCENT                                        GBIFPGM 
07979                              TO GCBH-BS-MH-PMT-LVL-POT-OTH        GBIFPGM 
07980         END-IF                                                    GBIFPGM 
07981      END-IF.                                                      GBIFPGM 
07982                                                                   GBIFPGM 
07983  4875-EXIT.                                                       GBIFPGM 
07984      EXIT.                                                        GBIFPGM 
07985 /                                                                 GBIFPGM 
07986 ******************************************************************GBIFPGM 
07987 *                                                                 GBIFPGM 
07988 *    B/C AND B/S MENTAL HEALTH PAYMENT LEVEL                      GBIFPGM 
07989 *                                                                 GBIFPGM 
07990 ******************************************************************GBIFPGM 
07991  4880-BLD-OP-BCBS-MH-PMT-LVL.                                     GBIFPGM 
07992                                                                   GBIFPGM 
07993      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMCI'                    GBIFPGM 
07994         IF WS-PROCESS-CON                                         GBIFPGM 
07995            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
07996                                       TO WS-PERCENT-VAL           GBIFPGM 
07997            MOVE WS-PERCENT       TO GCBH-OP-BCBS-MH-PMT-LVL-IN    GBIFPGM 
07998         END-IF                                                    GBIFPGM 
07999      END-IF                                                       GBIFPGM 
08000      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMCO'                    GBIFPGM 
08001         IF WS-PROCESS-GRP                                         GBIFPGM 
08002            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08003                                       TO WS-PERCENT-VAL           GBIFPGM 
08004            MOVE WS-PERCENT       TO GCBH-OP-BCBS-MH-PMT-LVL-OUT   GBIFPGM 
08005         END-IF                                                    GBIFPGM 
08006      END-IF                                                       GBIFPGM 
08007      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMCA'                    GBIFPGM 
08008         IF WS-PROCESS-CON                                         GBIFPGM 
08009            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08010                                          TO WS-PERCENT-VAL        GBIFPGM 
08011            MOVE WS-PERCENT       TO GCBH-OP-BCBS-MH-PMT-LVL-OTH   GBIFPGM 
08012         END-IF                                                    GBIFPGM 
08013      END-IF.                                                      GBIFPGM 
08014                                                                   GBIFPGM 
08015  4880-EXIT.                                                       GBIFPGM 
08016      EXIT.                                                        GBIFPGM 
08017 /                                                                 GBIFPGM 
08018 *MQ 10/03                                                         GBIFPGM 
08019 ******************************************************************GBIFPGM 
08020 *                                                                 GBIFPGM 
08021 *    B/C AND B/S MENTAL HEALTH PAYMENT LEVEL - ALL POT            GBIFPGM 
08022 *                                                                 GBIFPGM 
08023 ******************************************************************GBIFPGM 
08024  4885-BLD-BCBS-MH-PMT-LVL-POT.                                    GBIFPGM 
08025                                                                   GBIFPGM 
08026      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMAI'                    GBIFPGM 
08027         IF WS-PROCESS-CON                                         GBIFPGM 
08028            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08029                              TO WS-PERCENT-VAL                    GBIFPGM 
08030            MOVE WS-PERCENT                                        GBIFPGM 
08031                              TO GCBH-BCBS-MH-PMT-LVL-POT-IN       GBIFPGM 
08032         END-IF                                                    GBIFPGM 
08033      END-IF                                                       GBIFPGM 
08034      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMAO'                    GBIFPGM 
08035         IF WS-PROCESS-GRP                                         GBIFPGM 
08036            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08037                              TO WS-PERCENT-VAL                    GBIFPGM 
08038            MOVE WS-PERCENT                                        GBIFPGM 
08039                              TO GCBH-BCBS-MH-PMT-LVL-POT-OUT      GBIFPGM 
08040         END-IF                                                    GBIFPGM 
08041      END-IF                                                       GBIFPGM 
08042      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'AMAA'                    GBIFPGM 
08043         IF WS-PROCESS-CON                                         GBIFPGM 
08044            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08045                              TO WS-PERCENT-VAL                    GBIFPGM 
08046            MOVE WS-PERCENT                                        GBIFPGM 
08047                              TO GCBH-BCBS-MH-PMT-LVL-POT-OTH      GBIFPGM 
08048         END-IF                                                    GBIFPGM 
08049      END-IF.                                                      GBIFPGM 
08050                                                                   GBIFPGM 
08051  4885-EXIT.                                                       GBIFPGM 
08052      EXIT.                                                        GBIFPGM 
08053 /                                                                 GBIFPGM 
08054 ******************************************************************GBIFPGM 
08055 *                                                                 GBIFPGM 
08056 *    B/C MENTAL HEALTH BENEFIT PERIOD MAXIMUM                     GBIFPGM 
08057 *                                                                 GBIFPGM 
08058 ******************************************************************GBIFPGM 
08059  4890-BLD-OP-BC-MH-BP-MAX.                                        GBIFPGM 
08060                                                                   GBIFPGM 
08061      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMBI'                     GBIFPGM 
08062         IF WS-PROCESS-CON                                         GBIFPGM 
08063            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08064            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MH-BP-MAX-IN    GBIFPGM 
08065         END-IF                                                    GBIFPGM 
08066      END-IF                                                       GBIFPGM 
08067      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMBO'                     GBIFPGM 
08068         IF WS-PROCESS-GRP                                         GBIFPGM 
08069            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08070            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MH-BP-MAX-OUT   GBIFPGM 
08071         END-IF                                                    GBIFPGM 
08072      END-IF                                                       GBIFPGM 
08073      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMBA'                     GBIFPGM 
08074         IF WS-PROCESS-CON                                         GBIFPGM 
08075            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08076            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MH-BP-MAX-OTH   GBIFPGM 
08077         END-IF                                                    GBIFPGM 
08078      END-IF.                                                      GBIFPGM 
08079                                                                   GBIFPGM 
08080  4890-EXIT.                                                       GBIFPGM 
08081      EXIT.                                                        GBIFPGM 
08082 /                                                                 GBIFPGM 
08083 ******************************************************************GBIFPGM 
08084 *                                                                 GBIFPGM 
08085 *    B/S MENTAL HEALTH BENEFIT PERIOD MAXIMUM                     GBIFPGM 
08086 *                                                                 GBIFPGM 
08087 ******************************************************************GBIFPGM 
08088  4900-BLD-OP-BS-MH-BP-MAX.                                        GBIFPGM 
08089                                                                   GBIFPGM 
08090      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMBI'                     GBIFPGM 
08091         IF WS-PROCESS-CON                                         GBIFPGM 
08092            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08093            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MH-BP-MAX-IN    GBIFPGM 
08094         END-IF                                                    GBIFPGM 
08095      END-IF                                                       GBIFPGM 
08096      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMBO'                     GBIFPGM 
08097         IF WS-PROCESS-GRP                                         GBIFPGM 
08098            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08099            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MH-BP-MAX-OUT   GBIFPGM 
08100         END-IF                                                    GBIFPGM 
08101      END-IF                                                       GBIFPGM 
08102      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMBA'                     GBIFPGM 
08103         IF WS-PROCESS-CON                                         GBIFPGM 
08104            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08105            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MH-BP-MAX-OTH   GBIFPGM 
08106         END-IF                                                    GBIFPGM 
08107      END-IF.                                                      GBIFPGM 
08108                                                                   GBIFPGM 
08109  4900-EXIT.                                                       GBIFPGM 
08110      EXIT.                                                        GBIFPGM 
08111 /                                                                 GBIFPGM 
08112 ******************************************************************GBIFPGM 
08113 *                                                                 GBIFPGM 
08114 *    B/C AND B/S MENTAL HEALTH BENEFIT PERIOD MAXIMUM             GBIFPGM 
08115 *                                                                 GBIFPGM 
08116 ******************************************************************GBIFPGM 
08117  4910-BLD-OP-BCBS-MH-BP-MAX.                                      GBIFPGM 
08118                                                                   GBIFPGM 
08119      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMBI'                     GBIFPGM 
08120         IF WS-PROCESS-CON                                         GBIFPGM 
08121            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08122            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-MH-BP-MAX-IN  GBIFPGM 
08123         END-IF                                                    GBIFPGM 
08124      END-IF                                                       GBIFPGM 
08125      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMBO'                     GBIFPGM 
08126         IF WS-PROCESS-GRP                                         GBIFPGM 
08127            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08128            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-MH-BP-MAX-OUT GBIFPGM 
08129         END-IF                                                    GBIFPGM 
08130      END-IF                                                       GBIFPGM 
08131      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMBA'                     GBIFPGM 
08132         IF WS-PROCESS-CON                                         GBIFPGM 
08133            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08134            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-MH-BP-MAX-OTH GBIFPGM 
08135         END-IF                                                    GBIFPGM 
08136      END-IF.                                                      GBIFPGM 
08137                                                                   GBIFPGM 
08138  4910-EXIT.                                                       GBIFPGM 
08139      EXIT.                                                        GBIFPGM 
08140 /                                                                 GBIFPGM 
08141 *MQ 10/03                                                         GBIFPGM 
08142 ******************************************************************GBIFPGM 
08143 *                                                                 GBIFPGM 
08144 *    B/C AND B/S OUTPATIENT MH BENEFIT PERIOD MAXIMUM - CHILD     GBIFPGM 
08145 *                                                                 GBIFPGM 
08146 ******************************************************************GBIFPGM 
08147  4911-BLD-BCBS-OP-MH-BP-MAX-CH.                                   GBIFPGM 
08148                                                                   GBIFPGM 
08149      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACPI'                     GBIFPGM 
08150         IF WS-PROCESS-CON                                         GBIFPGM 
08151            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08152            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08153                         TO GCBH-BCBS-OP-MH-BP-MAX-CH-IN           GBIFPGM 
08154         END-IF                                                    GBIFPGM 
08155      END-IF                                                       GBIFPGM 
08156      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACPO'                     GBIFPGM 
08157         IF WS-PROCESS-GRP                                         GBIFPGM 
08158            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08159            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08160                         TO GCBH-BCBS-OP-MH-BP-MAX-CH-OUT          GBIFPGM 
08161         END-IF                                                    GBIFPGM 
08162      END-IF                                                       GBIFPGM 
08163      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACPA'                     GBIFPGM 
08164         IF WS-PROCESS-CON                                         GBIFPGM 
08165            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08166            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08167                         TO GCBH-BCBS-OP-MH-BP-MAX-CH-OTH          GBIFPGM 
08168         END-IF                                                    GBIFPGM 
08169      END-IF.                                                      GBIFPGM 
08170                                                                   GBIFPGM 
08171  4911-EXIT.                                                       GBIFPGM 
08172      EXIT.                                                        GBIFPGM 
08173 /                                                                 GBIFPGM 
08174 *MQ 10/03                                                         GBIFPGM 
08175 ******************************************************************GBIFPGM 
08176 *                                                                 GBIFPGM 
08177 *    B/C AND B/S OUTPATIENT MH BENEFIT PERIOD MAXIMUM - ADULT     GBIFPGM 
08178 *                                                                 GBIFPGM 
08179 ******************************************************************GBIFPGM 
08180  4915-BLD-BCBS-OP-MH-BP-MAX-AD.                                   GBIFPGM 
08181                                                                   GBIFPGM 
08182      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AAPI'                     GBIFPGM 
08183         IF WS-PROCESS-CON                                         GBIFPGM 
08184            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08185            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08186                         TO GCBH-BCBS-OP-MH-BP-MAX-AD-IN           GBIFPGM 
08187         END-IF                                                    GBIFPGM 
08188      END-IF                                                       GBIFPGM 
08189      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AAPO'                     GBIFPGM 
08190         IF WS-PROCESS-GRP                                         GBIFPGM 
08191            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08192            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08193                         TO GCBH-BCBS-OP-MH-BP-MAX-AD-OUT          GBIFPGM 
08194         END-IF                                                    GBIFPGM 
08195      END-IF                                                       GBIFPGM 
08196      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AAPA'                     GBIFPGM 
08197         IF WS-PROCESS-CON                                         GBIFPGM 
08198            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08199            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08200                         TO GCBH-BCBS-OP-MH-BP-MAX-AD-OTH          GBIFPGM 
08201         END-IF                                                    GBIFPGM 
08202      END-IF.                                                      GBIFPGM 
08203                                                                   GBIFPGM 
08204  4915-EXIT.                                                       GBIFPGM 
08205      EXIT.                                                        GBIFPGM 
08206 /                                                                 GBIFPGM 
08207 ******************************************************************GBIFPGM 
08208 *                                                                 GBIFPGM 
08209 *    B/C MENTAL HEALTH LIFETIME MAXIMUM                           GBIFPGM 
08210 *                                                                 GBIFPGM 
08211 ******************************************************************GBIFPGM 
08212  4920-BLD-OP-BC-MH-LIFE-MAX.                                      GBIFPGM 
08213                                                                   GBIFPGM 
08214      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMOI'                     GBIFPGM 
08215         IF WS-PROCESS-CON                                         GBIFPGM 
08216            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08217            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MH-LIFE-MAX-IN  GBIFPGM 
08218         END-IF                                                    GBIFPGM 
08219      END-IF                                                       GBIFPGM 
08220      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMOO'                     GBIFPGM 
08221         IF WS-PROCESS-GRP                                         GBIFPGM 
08222            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08223            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MH-LIFE-MAX-OUT GBIFPGM 
08224         END-IF                                                    GBIFPGM 
08225      END-IF                                                       GBIFPGM 
08226      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CMOA'                     GBIFPGM 
08227         IF WS-PROCESS-CON                                         GBIFPGM 
08228            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08229            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-MH-LIFE-MAX-OTH GBIFPGM 
08230         END-IF                                                    GBIFPGM 
08231      END-IF.                                                      GBIFPGM 
08232                                                                   GBIFPGM 
08233  4920-EXIT.                                                       GBIFPGM 
08234      EXIT.                                                        GBIFPGM 
08235 /                                                                 GBIFPGM 
10044 *                                                                 GBIFPGM 
08236 ******************************************************************GBIFPGM 
08237 *                                                                 GBIFPGM 
08238 *    B/S MENTAL HEALTH LIFETIME MAXIMUM                           GBIFPGM 
08239 *                                                                 GBIFPGM 
08240 ******************************************************************GBIFPGM 
10050 *4930-BLD-OP-BS-MH-LIFE-MAX.                                      GBIFPGM 
10051 *                                                                 GBIFPGM 
10052 *    IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMOI'                     GBIFPGM 
10053 *       IF WS-PROCESS-CON                                         GBIFPGM 
10054 *          PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10055 *          MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MH-LIFE-MAX-IN  GBIFPGM 
10056 *       END-IF                                                    GBIFPGM 
10057 *    END-IF                                                       GBIFPGM 
10058 *    IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMOO'                     GBIFPGM 
10059 *       IF WS-PROCESS-GRP                                         GBIFPGM 
10060 *          PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10061 *          MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MH-LIFE-MAX-OUT GBIFPGM 
10062 *       END-IF                                                    GBIFPGM 
10063 *    END-IF                                                       GBIFPGM 
10064 *    IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMOA'                     GBIFPGM 
10065 *       IF WS-PROCESS-CON                                         GBIFPGM 
10066 *          PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10067 *          MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-MH-LIFE-MAX-OTH GBIFPGM 
10068 *       END-IF                                                    GBIFPGM 
10069 *    END-IF.                                                      GBIFPGM 
10070 *                                                                 GBIFPGM 
10071 *4930-EXIT.                                                       GBIFPGM 
10072 *    EXIT.                                                        GBIFPGM 
08264 /                                                                 GBIFPGM 
08265 ******************************************************************GBIFPGM 
08266 *                                                                 GBIFPGM 
08267 *    B/C AND B/S MENTAL HEALTH LIFETIME MAXIMUM                   GBIFPGM 
08268 *                                                                 GBIFPGM 
08269 ******************************************************************GBIFPGM 
08270  4940-BLD-OP-BCBS-MH-LIFE-MAX.                                    GBIFPGM 
08271                                                                   GBIFPGM 
08272      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMOI'                     GBIFPGM 
08273         IF WS-PROCESS-CON                                         GBIFPGM 
08274            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08275            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-MH-LIFE-MAX-INGBIFPGM 
08276         END-IF                                                    GBIFPGM 
08277      END-IF                                                       GBIFPGM 
08278      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMOO'                     GBIFPGM 
08279         IF WS-PROCESS-GRP                                         GBIFPGM 
08280            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08281            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08282                               TO GCBH-OP-BCBS-MH-LIFE-MAX-OUT     GBIFPGM 
08283         END-IF                                                    GBIFPGM 
08284      END-IF                                                       GBIFPGM 
08285      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AMOA'                     GBIFPGM 
08286         IF WS-PROCESS-CON                                         GBIFPGM 
08287            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08288            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08289                               TO GCBH-OP-BCBS-MH-LIFE-MAX-OTH     GBIFPGM 
08290         END-IF                                                    GBIFPGM 
08291      END-IF.                                                      GBIFPGM 
08292                                                                   GBIFPGM 
08293  4940-EXIT.                                                       GBIFPGM 
08294      EXIT.                                                        GBIFPGM 
08295 /                                                                 GBIFPGM 
08296 ******************************************************************GBIFPGM 
08297 *                                                                 GBIFPGM 
08298 *    B/C SUBSTANCE ABUSE PAYMENT LEVEL                            GBIFPGM 
08299 *                                                                 GBIFPGM 
08300 ******************************************************************GBIFPGM 
08301  4950-BLD-OP-BC-SA-PMT-LVL.                                       GBIFPGM 
08302                                                                   GBIFPGM 
08303      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSCI'                    GBIFPGM 
08304         IF WS-PROCESS-CON                                         GBIFPGM 
08305            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08306                                       TO WS-PERCENT-VAL           GBIFPGM 
08307            MOVE WS-PERCENT       TO GCBH-OP-BC-SA-PMT-LVL-IN      GBIFPGM 
08308         END-IF                                                    GBIFPGM 
08309      END-IF                                                       GBIFPGM 
08310      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSCO'                    GBIFPGM 
08311         IF WS-PROCESS-GRP                                         GBIFPGM 
08312            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08313                                       TO WS-PERCENT-VAL           GBIFPGM 
08314            MOVE WS-PERCENT       TO GCBH-OP-BC-SA-PMT-LVL-OUT     GBIFPGM 
08315         END-IF                                                    GBIFPGM 
08316      END-IF                                                       GBIFPGM 
08317      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSCA'                    GBIFPGM 
08318         IF WS-PROCESS-CON                                         GBIFPGM 
08319            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08320                                          TO WS-PERCENT-VAL        GBIFPGM 
08321            MOVE WS-PERCENT       TO GCBH-OP-BC-SA-PMT-LVL-OTH     GBIFPGM 
08322         END-IF                                                    GBIFPGM 
08323      END-IF.                                                      GBIFPGM 
08324                                                                   GBIFPGM 
08325  4950-EXIT.                                                       GBIFPGM 
08326      EXIT.                                                        GBIFPGM 
08327 /                                                                 GBIFPGM 
08328 *MQ 10/03                                                         GBIFPGM 
08329 ******************************************************************GBIFPGM 
08330 *                                                                 GBIFPGM 
08331 *    B/C SUBSTANCE ABUSE PAYMENT LEVEL - ALL POT                  GBIFPGM 
08332 *                                                                 GBIFPGM 
08333 ******************************************************************GBIFPGM 
08334  4955-BLD-BC-SA-PMT-LVL-POT.                                      GBIFPGM 
08335                                                                   GBIFPGM 
08336      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSAI'                    GBIFPGM 
08337         IF WS-PROCESS-CON                                         GBIFPGM 
08338            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08339                                       TO WS-PERCENT-VAL           GBIFPGM 
08340            MOVE WS-PERCENT       TO GCBH-BC-SA-PMT-LVL-POT-IN     GBIFPGM 
08341         END-IF                                                    GBIFPGM 
08342      END-IF                                                       GBIFPGM 
08343      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSAO'                    GBIFPGM 
08344         IF WS-PROCESS-GRP                                         GBIFPGM 
08345            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08346                                       TO WS-PERCENT-VAL           GBIFPGM 
08347            MOVE WS-PERCENT       TO GCBH-BC-SA-PMT-LVL-POT-OUT    GBIFPGM 
08348         END-IF                                                    GBIFPGM 
08349      END-IF                                                       GBIFPGM 
08350      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'CSAA'                    GBIFPGM 
08351         IF WS-PROCESS-CON                                         GBIFPGM 
08352            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08353                                          TO WS-PERCENT-VAL        GBIFPGM 
08354            MOVE WS-PERCENT       TO GCBH-BC-SA-PMT-LVL-POT-OTH    GBIFPGM 
08355         END-IF                                                    GBIFPGM 
08356      END-IF.                                                      GBIFPGM 
08357                                                                   GBIFPGM 
08358  4955-EXIT.                                                       GBIFPGM 
08359      EXIT.                                                        GBIFPGM 
08360 /                                                                 GBIFPGM 
08361 ******************************************************************GBIFPGM 
08362 *                                                                 GBIFPGM 
08363 *    B/S SUBSTANCE ABUSE PAYMENT LEVEL                            GBIFPGM 
08364 *                                                                 GBIFPGM 
08365 ******************************************************************GBIFPGM 
08366  4960-BLD-OP-BS-SA-PMT-LVL.                                       GBIFPGM 
08367                                                                   GBIFPGM 
08368      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSCI'                    GBIFPGM 
08369         IF WS-PROCESS-CON                                         GBIFPGM 
08370            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08371                                       TO WS-PERCENT-VAL           GBIFPGM 
08372            MOVE WS-PERCENT            TO GCBH-OP-BS-SA-PMT-LVL-IN GBIFPGM 
08373         END-IF                                                    GBIFPGM 
08374      END-IF                                                       GBIFPGM 
08375      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSCO'                    GBIFPGM 
08376         IF WS-PROCESS-GRP                                         GBIFPGM 
08377            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08378                                       TO WS-PERCENT-VAL           GBIFPGM 
08379            MOVE WS-PERCENT            TO GCBH-OP-BS-SA-PMT-LVL-OUTGBIFPGM 
08380         END-IF                                                    GBIFPGM 
08381      END-IF                                                       GBIFPGM 
08382      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSCA'                    GBIFPGM 
08383         IF WS-PROCESS-CON                                         GBIFPGM 
08384            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08385                                          TO WS-PERCENT-VAL        GBIFPGM 
08386            MOVE WS-PERCENT       TO GCBH-OP-BS-SA-PMT-LVL-OTH     GBIFPGM 
08387         END-IF                                                    GBIFPGM 
08388      END-IF.                                                      GBIFPGM 
08389                                                                   GBIFPGM 
08390  4960-EXIT.                                                       GBIFPGM 
08391      EXIT.                                                        GBIFPGM 
08392 /                                                                 GBIFPGM 
08393 *MQ 10/03                                                         GBIFPGM 
08394 ******************************************************************GBIFPGM 
08395 *                                                                 GBIFPGM 
08396 *    B/S SUBSTANCE ABUSE PAYMENT LEVEL - ALL POT                  GBIFPGM 
08397 *                                                                 GBIFPGM 
08398 ******************************************************************GBIFPGM 
08399  4965-BLD-BS-SA-PMT-LVL-POT.                                      GBIFPGM 
08400                                                                   GBIFPGM 
08401      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSAI'                    GBIFPGM 
08402         IF WS-PROCESS-CON                                         GBIFPGM 
08403            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08404                               TO WS-PERCENT-VAL                   GBIFPGM 
08405            MOVE WS-PERCENT    TO GCBH-BS-SA-PMT-LVL-POT-IN        GBIFPGM 
08406         END-IF                                                    GBIFPGM 
08407      END-IF                                                       GBIFPGM 
08408      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSAO'                    GBIFPGM 
08409         IF WS-PROCESS-GRP                                         GBIFPGM 
08410            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08411                               TO WS-PERCENT-VAL                   GBIFPGM 
08412            MOVE WS-PERCENT    TO GCBH-BS-SA-PMT-LVL-POT-OUT       GBIFPGM 
08413         END-IF                                                    GBIFPGM 
08414      END-IF                                                       GBIFPGM 
08415      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'SSAA'                    GBIFPGM 
08416         IF WS-PROCESS-CON                                         GBIFPGM 
08417            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08418                               TO WS-PERCENT-VAL                   GBIFPGM 
08419            MOVE WS-PERCENT    TO GCBH-BS-SA-PMT-LVL-POT-OTH       GBIFPGM 
08420         END-IF                                                    GBIFPGM 
08421      END-IF.                                                      GBIFPGM 
08422                                                                   GBIFPGM 
08423  4965-EXIT.                                                       GBIFPGM 
08424      EXIT.                                                        GBIFPGM 
08425 /                                                                 GBIFPGM 
08426 ******************************************************************GBIFPGM 
08427 *                                                                 GBIFPGM 
08428 *    B/C AND B/S SUBSTANCE ABUSE PAYMENT LEVEL                    GBIFPGM 
08429 *                                                                 GBIFPGM 
08430 ******************************************************************GBIFPGM 
08431  4970-BLD-OP-BCBS-SA-PMT-LVL.                                     GBIFPGM 
08432                                                                   GBIFPGM 
08433      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASCI'                    GBIFPGM 
08434         IF WS-PROCESS-CON                                         GBIFPGM 
08435            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08436                                       TO WS-PERCENT-VAL           GBIFPGM 
08437            MOVE WS-PERCENT       TO GCBH-OP-BCBS-SA-PMT-LVL-IN    GBIFPGM 
08438         END-IF                                                    GBIFPGM 
08439      END-IF                                                       GBIFPGM 
08440      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASCO'                    GBIFPGM 
08441         IF WS-PROCESS-GRP                                         GBIFPGM 
08442            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08443                                       TO WS-PERCENT-VAL           GBIFPGM 
08444            MOVE WS-PERCENT       TO GCBH-OP-BCBS-SA-PMT-LVL-OUT   GBIFPGM 
08445         END-IF                                                    GBIFPGM 
08446      END-IF                                                       GBIFPGM 
08447      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASCA'                    GBIFPGM 
08448         IF WS-PROCESS-CON                                         GBIFPGM 
08449            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08450                                          TO WS-PERCENT-VAL        GBIFPGM 
08451            MOVE WS-PERCENT       TO GCBH-OP-BCBS-SA-PMT-LVL-OTH   GBIFPGM 
08452         END-IF                                                    GBIFPGM 
08453      END-IF.                                                      GBIFPGM 
08454                                                                   GBIFPGM 
08455  4970-EXIT.                                                       GBIFPGM 
08456      EXIT.                                                        GBIFPGM 
08457 /                                                                 GBIFPGM 
08458 *MQ 10/03                                                         GBIFPGM 
08459 ******************************************************************GBIFPGM 
08460 *                                                                 GBIFPGM 
08461 *    B/C AND B/S SUBSTANCE ABUSE PAYMENT LEVEL - ALL POT          GBIFPGM 
08462 *                                                                 GBIFPGM 
08463 ******************************************************************GBIFPGM 
08464  4975-BLD-BCBS-SA-PMT-LVL-POT.                                    GBIFPGM 
08465                                                                   GBIFPGM 
08466      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASAI'                    GBIFPGM 
08467         IF WS-PROCESS-CON                                         GBIFPGM 
08468            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08469                                 TO WS-PERCENT-VAL                 GBIFPGM 
08470            MOVE WS-PERCENT      TO GCBH-BCBS-SA-PMT-LVL-POT-IN    GBIFPGM 
08471         END-IF                                                    GBIFPGM 
08472      END-IF                                                       GBIFPGM 
08473      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASAO'                    GBIFPGM 
08474         IF WS-PROCESS-GRP                                         GBIFPGM 
08475            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08476                                 TO WS-PERCENT-VAL                 GBIFPGM 
08477            MOVE WS-PERCENT      TO GCBH-BCBS-SA-PMT-LVL-POT-OUT   GBIFPGM 
08478         END-IF                                                    GBIFPGM 
08479      END-IF                                                       GBIFPGM 
08480      IF GAB-COINS-ACCUMID (GAB-INDEX) = 'ASAA'                    GBIFPGM 
08481         IF WS-PROCESS-CON                                         GBIFPGM 
08482            MOVE GAB-COINS-PERCENT-LEVEL (GAB-INDEX)               GBIFPGM 
08483                                 TO WS-PERCENT-VAL                 GBIFPGM 
08484            MOVE WS-PERCENT      TO GCBH-BCBS-SA-PMT-LVL-POT-OTH   GBIFPGM 
08485         END-IF                                                    GBIFPGM 
08486      END-IF.                                                      GBIFPGM 
08487                                                                   GBIFPGM 
08488  4975-EXIT.                                                       GBIFPGM 
08489      EXIT.                                                        GBIFPGM 
08490 /                                                                 GBIFPGM 
08491 ******************************************************************GBIFPGM 
08492 *                                                                 GBIFPGM 
08493 *    B/C SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM                   GBIFPGM 
08494 *                                                                 GBIFPGM 
08495 ******************************************************************GBIFPGM 
08496  4980-BLD-OP-BC-SA-BEN-PERD-MAX.                                  GBIFPGM 
08497                                                                   GBIFPGM 
08498      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSBI'                     GBIFPGM 
08499         IF WS-PROCESS-CON                                         GBIFPGM 
08500            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08501            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-SA-BP-MAX-IN    GBIFPGM 
08502         END-IF                                                    GBIFPGM 
08503      END-IF                                                       GBIFPGM 
08504      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSBO'                     GBIFPGM 
08505         IF WS-PROCESS-GRP                                         GBIFPGM 
08506            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08507            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-SA-BP-MAX-OUT   GBIFPGM 
08508         END-IF                                                    GBIFPGM 
08509      END-IF                                                       GBIFPGM 
08510      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSBA'                     GBIFPGM 
08511         IF WS-PROCESS-CON                                         GBIFPGM 
08512            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08513            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-SA-BP-MAX-OTH   GBIFPGM 
08514         END-IF                                                    GBIFPGM 
08515      END-IF.                                                      GBIFPGM 
08516                                                                   GBIFPGM 
08517  4980-EXIT.                                                       GBIFPGM 
08518      EXIT.                                                        GBIFPGM 
08519 /                                                                 GBIFPGM 
08520 ******************************************************************GBIFPGM 
08521 *                                                                 GBIFPGM 
08522 *    B/S SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM                   GBIFPGM 
08523 *                                                                 GBIFPGM 
08524 ******************************************************************GBIFPGM 
08525  4990-BLD-OP-BS-SA-BEN-PERD-MAX.                                  GBIFPGM 
08526                                                                   GBIFPGM 
08527      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSBI'                     GBIFPGM 
08528         IF WS-PROCESS-CON                                         GBIFPGM 
08529            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08530            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-SA-BP-MAX-IN    GBIFPGM 
08531         END-IF                                                    GBIFPGM 
08532      END-IF                                                       GBIFPGM 
08533      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSBO'                     GBIFPGM 
08534         IF WS-PROCESS-GRP                                         GBIFPGM 
08535            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08536            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-SA-BP-MAX-OUT   GBIFPGM 
08537         END-IF                                                    GBIFPGM 
08538      END-IF                                                       GBIFPGM 
08539      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSBA'                     GBIFPGM 
08540         IF WS-PROCESS-CON                                         GBIFPGM 
08541            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08542            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-SA-BP-MAX-OTH   GBIFPGM 
08543         END-IF                                                    GBIFPGM 
08544      END-IF.                                                      GBIFPGM 
08545                                                                   GBIFPGM 
08546  4990-EXIT.                                                       GBIFPGM 
08547      EXIT.                                                        GBIFPGM 
08548 /                                                                 GBIFPGM 
08549 ******************************************************************GBIFPGM 
08550 *                                                                 GBIFPGM 
08551 *    B/C AND B/S SUBSTANCE ABUSE BENEFIT PERIOD MAXIMUM           GBIFPGM 
08552 *                                                                 GBIFPGM 
08553 ******************************************************************GBIFPGM 
08554  5000-BLD-OP-BCBS-SA-BP-MAX.                                      GBIFPGM 
08555                                                                   GBIFPGM 
08556      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASBI'                     GBIFPGM 
08557         IF WS-PROCESS-CON                                         GBIFPGM 
08558            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08559            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-SA-BP-MAX-IN  GBIFPGM 
08560         END-IF                                                    GBIFPGM 
08561      END-IF                                                       GBIFPGM 
08562      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASBO'                     GBIFPGM 
08563         IF WS-PROCESS-GRP                                         GBIFPGM 
08564            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08565            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-SA-BP-MAX-OUT GBIFPGM 
08566         END-IF                                                    GBIFPGM 
08567      END-IF                                                       GBIFPGM 
08568      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASBA'                     GBIFPGM 
08569         IF WS-PROCESS-CON                                         GBIFPGM 
08570            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08571            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-SA-BP-MAX-OTH GBIFPGM 
08572         END-IF                                                    GBIFPGM 
08573      END-IF.                                                      GBIFPGM 
08574                                                                   GBIFPGM 
08575  5000-EXIT.                                                       GBIFPGM 
08576      EXIT.                                                        GBIFPGM 
08577 /                                                                 GBIFPGM 
08578 *MQ 10/03                                                         GBIFPGM 
08579 ******************************************************************GBIFPGM 
08580 *                                                                 GBIFPGM 
08581 *    B/C AND B/S OUTPATIENT SUB ABUSE BENEFIT PERIOD MAXIMUM-CHILDGBIFPGM 
08582 *                                                                 GBIFPGM 
08583 ******************************************************************GBIFPGM 
08584  5002-BLD-BCBS-OP-SA-BP-MAX-CH.                                   GBIFPGM 
08585                                                                   GBIFPGM 
08586      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACSI'                     GBIFPGM 
08587         IF WS-PROCESS-CON                                         GBIFPGM 
08588            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08589            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08590                         TO GCBH-BCBS-OP-SA-BP-MAX-CH-IN           GBIFPGM 
08591         END-IF                                                    GBIFPGM 
08592      END-IF                                                       GBIFPGM 
08593      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACSO'                     GBIFPGM 
08594         IF WS-PROCESS-GRP                                         GBIFPGM 
08595            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08596            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08597                         TO GCBH-BCBS-OP-SA-BP-MAX-CH-OUT          GBIFPGM 
08598         END-IF                                                    GBIFPGM 
08599      END-IF                                                       GBIFPGM 
08600      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ACSA'                     GBIFPGM 
08601         IF WS-PROCESS-CON                                         GBIFPGM 
08602            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08603            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08604                         TO GCBH-BCBS-OP-SA-BP-MAX-CH-OTH          GBIFPGM 
08605         END-IF                                                    GBIFPGM 
08606      END-IF.                                                      GBIFPGM 
08607                                                                   GBIFPGM 
08608  5002-EXIT.                                                       GBIFPGM 
08609      EXIT.                                                        GBIFPGM 
08610 /                                                                 GBIFPGM 
08611 *MQ 10/03                                                         GBIFPGM 
08612 ******************************************************************GBIFPGM 
08613 *                                                                 GBIFPGM 
08614 *    B/C AND B/S OUTPATIENT SUB ABUSE BENEFIT PERIOD MAXIMUM-ADULTGBIFPGM 
08615 *                                                                 GBIFPGM 
08616 ******************************************************************GBIFPGM 
08617  5005-BLD-BCBS-OP-SA-BP-MAX-AD.                                   GBIFPGM 
08618                                                                   GBIFPGM 
08619      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AASI'                     GBIFPGM 
08620         IF WS-PROCESS-CON                                         GBIFPGM 
08621            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08622            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08623                         TO GCBH-BCBS-OP-SA-BP-MAX-AD-IN           GBIFPGM 
08624         END-IF                                                    GBIFPGM 
08625      END-IF                                                       GBIFPGM 
08626      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AASO'                     GBIFPGM 
08627         IF WS-PROCESS-GRP                                         GBIFPGM 
08628            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08629            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08630                         TO GCBH-BCBS-OP-SA-BP-MAX-AD-OUT          GBIFPGM 
08631         END-IF                                                    GBIFPGM 
08632      END-IF                                                       GBIFPGM 
08633      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'AASA'                     GBIFPGM 
08634         IF WS-PROCESS-CON                                         GBIFPGM 
08635            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08636            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08637                         TO GCBH-BCBS-OP-SA-BP-MAX-AD-OTH          GBIFPGM 
08638         END-IF                                                    GBIFPGM 
08639      END-IF.                                                      GBIFPGM 
08640                                                                   GBIFPGM 
08641  5005-EXIT.                                                       GBIFPGM 
08642      EXIT.                                                        GBIFPGM 
08643 /                                                                 GBIFPGM 
08644 ******************************************************************GBIFPGM 
08645 *                                                                 GBIFPGM 
08646 *    B/C SUBSTANCE ABUSE LIFETIME MAXIMUM                         GBIFPGM 
08647 *                                                                 GBIFPGM 
08648 ******************************************************************GBIFPGM 
08649  5010-BLD-OP-BC-SA-LIFETIME-MAX.                                  GBIFPGM 
08650                                                                   GBIFPGM 
08651      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSOI'                     GBIFPGM 
08652         IF WS-PROCESS-CON                                         GBIFPGM 
08653            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08654            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-SA-LIFE-MAX-IN  GBIFPGM 
08655         END-IF                                                    GBIFPGM 
08656      END-IF                                                       GBIFPGM 
08657      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSOO'                     GBIFPGM 
08658         IF WS-PROCESS-GRP                                         GBIFPGM 
08659            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08660            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-SA-LIFE-MAX-OUT GBIFPGM 
08661         END-IF                                                    GBIFPGM 
08662      END-IF                                                       GBIFPGM 
08663      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'CSOA'                     GBIFPGM 
08664         IF WS-PROCESS-CON                                         GBIFPGM 
08665            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08666            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BC-SA-LIFE-MAX-OTH GBIFPGM 
08667         END-IF                                                    GBIFPGM 
08668      END-IF.                                                      GBIFPGM 
08669                                                                   GBIFPGM 
08670  5010-EXIT.                                                       GBIFPGM 
08671      EXIT.                                                        GBIFPGM 
08672 /                                                                 GBIFPGM 
08673 ******************************************************************GBIFPGM 
08674 *                                                                 GBIFPGM 
08675 *    B/S SUBSTANCE ABUSE LIFETIME MAXIMUM                         GBIFPGM 
08676 *                                                                 GBIFPGM 
08677 ******************************************************************GBIFPGM 
08678  5020-BLD-OP-BS-SA-LIFETIME-MAX.                                  GBIFPGM 
08679                                                                   GBIFPGM 
08680      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSOI'                     GBIFPGM 
08681         IF WS-PROCESS-CON                                         GBIFPGM 
08682            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08683            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-SA-LIFE-MAX-IN  GBIFPGM 
08684         END-IF                                                    GBIFPGM 
08685      END-IF                                                       GBIFPGM 
08686      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSOO'                     GBIFPGM 
08687         IF WS-PROCESS-GRP                                         GBIFPGM 
08688            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08689            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-SA-LIFE-MAX-OUT GBIFPGM 
08690         END-IF                                                    GBIFPGM 
08691      END-IF                                                       GBIFPGM 
08692      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SSOA'                     GBIFPGM 
08693         IF WS-PROCESS-CON                                         GBIFPGM 
08694            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08695            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BS-SA-LIFE-MAX-OTH GBIFPGM 
08696         END-IF                                                    GBIFPGM 
08697      END-IF.                                                      GBIFPGM 
08698                                                                   GBIFPGM 
08699  5020-EXIT.                                                       GBIFPGM 
08700      EXIT.                                                        GBIFPGM 
08701 /                                                                 GBIFPGM 
08702 ******************************************************************GBIFPGM 
08703 *                                                                 GBIFPGM 
08704 *    B/C AND B/S SUBSTANCE ABUSE LIFETIME MAXIMUM                 GBIFPGM 
08705 *                                                                 GBIFPGM 
08706 ******************************************************************GBIFPGM 
08707  5030-BLD-OP-BCBS-SA-LIFE-MAX.                                    GBIFPGM 
08708                                                                   GBIFPGM 
08709      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASOI'                     GBIFPGM 
08710         IF WS-PROCESS-CON                                         GBIFPGM 
08711            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08712            MOVE WS-TEMP-VALUE-LIMIT TO GCBH-OP-BCBS-SA-LIFE-MAX-INGBIFPGM 
08713         END-IF                                                    GBIFPGM 
08714      END-IF                                                       GBIFPGM 
08715      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASOO'                     GBIFPGM 
08716         IF WS-PROCESS-GRP                                         GBIFPGM 
08717            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08718            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08719                               TO GCBH-OP-BCBS-SA-LIFE-MAX-OUT     GBIFPGM 
08720         END-IF                                                    GBIFPGM 
08721      END-IF                                                       GBIFPGM 
08722      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'ASOA'                     GBIFPGM 
08723         IF WS-PROCESS-CON                                         GBIFPGM 
08724            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08725            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08726                               TO GCBH-OP-BCBS-SA-LIFE-MAX-OTH     GBIFPGM 
08727         END-IF                                                    GBIFPGM 
08728      END-IF.                                                      GBIFPGM 
08729                                                                   GBIFPGM 
08730  5030-EXIT.                                                       GBIFPGM 
08731      EXIT.                                                        GBIFPGM 
08732 /                                                                 GBIFPGM 
08733 *MQ 10/03                                                         GBIFPGM 
08734 ******************************************************************GBIFPGM 
08735 *                                                                 GBIFPGM 
08736 *    HOSPICE CARE BENEFIT PERIOD MAXIMUM                          GBIFPGM 
08737 *                                                                 GBIFPGM 
08738 ******************************************************************GBIFPGM 
08739  5040-BLD-HOSP-CARE-BP-MAX.                                       GBIFPGM 
08740                                                                   GBIFPGM 
08741      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HOLI'                     GBIFPGM 
08742         IF WS-PROCESS-CON                                         GBIFPGM 
08743            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08744            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08745                               TO GCBH-HOSP-CARE-BP-MAX-IN         GBIFPGM 
08746         END-IF                                                    GBIFPGM 
08747      END-IF                                                       GBIFPGM 
08748      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HOLO'                     GBIFPGM 
08749         IF WS-PROCESS-GRP                                         GBIFPGM 
08750            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08751            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08752                               TO GCBH-HOSP-CARE-BP-MAX-OUT        GBIFPGM 
08753         END-IF                                                    GBIFPGM 
08754      END-IF                                                       GBIFPGM 
08755      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HOLA'                     GBIFPGM 
08756         IF WS-PROCESS-CON                                         GBIFPGM 
08757            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08758            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08759                               TO GCBH-HOSP-CARE-BP-MAX-OTH        GBIFPGM 
08760         END-IF                                                    GBIFPGM 
08761      END-IF.                                                      GBIFPGM 
08762                                                                   GBIFPGM 
08763  5040-EXIT.                                                       GBIFPGM 
08764      EXIT.                                                        GBIFPGM 
08765 /                                                                 GBIFPGM 
08766 *MQ 10/03                                                         GBIFPGM 
08767 ******************************************************************GBIFPGM 
08768 *                                                                 GBIFPGM 
08769 *    COORDINATED HOME CARE BENEFIT PERIOD MAXIMUM                 GBIFPGM 
08770 *                                                                 GBIFPGM 
08771 ******************************************************************GBIFPGM 
08772  5050-BLD-COOR-HM-CARE-BP-MAX.                                    GBIFPGM 
08773                                                                   GBIFPGM 
08774      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HHCI'                     GBIFPGM 
08775         IF WS-PROCESS-CON                                         GBIFPGM 
08776            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08777            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08778                               TO GCBH-COOR-HM-CARE-BP-MAX-IN      GBIFPGM 
08779         END-IF                                                    GBIFPGM 
08780      END-IF                                                       GBIFPGM 
08781      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HHCO'                     GBIFPGM 
08782         IF WS-PROCESS-GRP                                         GBIFPGM 
08783            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08784            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08785                               TO GCBH-COOR-HM-CARE-BP-MAX-OUT     GBIFPGM 
08786         END-IF                                                    GBIFPGM 
08787      END-IF                                                       GBIFPGM 
08788      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HHCA'                     GBIFPGM 
08789         IF WS-PROCESS-CON                                         GBIFPGM 
08790            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
08791            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
08792                               TO GCBH-COOR-HM-CARE-BP-MAX-OTH     GBIFPGM 
08793         END-IF                                                    GBIFPGM 
08794      END-IF.                                                      GBIFPGM 
08795                                                                   GBIFPGM 
08796  5050-EXIT.                                                       GBIFPGM 
08797      EXIT.                                                        GBIFPGM 
08798 /                                                                 GBIFPGM 
10608 *MQ 5/11/05                                                       GBIFPGM 
08799 ******************************************************************GBIFPGM 
08800 *                                                                 GBIFPGM 
10611 *    COORDINATED HOME CARE BENEFIT DAY MAXIMUM                    GBIFPGM 
10612 *                                                                 GBIFPGM 
10613 ******************************************************************GBIFPGM 
10614  5052-BLD-COOR-HM-CARE-DAY-MAX.                                   GBIFPGM 
10615                                                                   GBIFPGM 
10616      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HHDI'                     GBIFPGM 
10617         IF WS-PROCESS-CON                                         GBIFPGM 
10618            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10619            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
10620                               TO GCBH-COOR-HM-CARE-DAY-MAX-IN     GBIFPGM 
10621         END-IF                                                    GBIFPGM 
10622      END-IF                                                       GBIFPGM 
10623      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HHDO'                     GBIFPGM 
10624         IF WS-PROCESS-GRP                                         GBIFPGM 
10625            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10626            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
10627                               TO GCBH-COOR-HM-CARE-DAY-MAX-OUT    GBIFPGM 
10628         END-IF                                                    GBIFPGM 
10629      END-IF                                                       GBIFPGM 
10630      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'HHDA'                     GBIFPGM 
10631         IF WS-PROCESS-CON                                         GBIFPGM 
10632            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10633            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
10634                               TO GCBH-COOR-HM-CARE-DAY-MAX-OTH    GBIFPGM 
10635         END-IF                                                    GBIFPGM 
10636      END-IF.                                                      GBIFPGM 
10637                                                                   GBIFPGM 
10638  5052-EXIT.                                                       GBIFPGM 
10639      EXIT.                                                        GBIFPGM 
10640 /                                                                 GBIFPGM 
10641 ******************************************************************GBIFPGM 
10642 *                                                                 GBIFPGM 
10643 *    SERIOUS MENTAL ILLNESS BENEFIT PERIOD MAXIMUM - INPATIENT    GBIFPGM 
10644 *                                                                 GBIFPGM 
10645 ******************************************************************GBIFPGM 
10646  5060-BLD-SMI-BP-MAX-IP.                                          GBIFPGM 
10647                                                                   GBIFPGM 
10648      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMII'                     GBIFPGM 
10649         IF WS-PROCESS-CON                                         GBIFPGM 
10650            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10651            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
10652                               TO GCBH-SMI-BP-MAX-IP-IN            GBIFPGM 
10653         END-IF                                                    GBIFPGM 
10654      END-IF                                                       GBIFPGM 
10655      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMIO'                     GBIFPGM 
10656         IF WS-PROCESS-GRP                                         GBIFPGM 
10657            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10658            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
10659                               TO GCBH-SMI-BP-MAX-IP-OUT           GBIFPGM 
10660         END-IF                                                    GBIFPGM 
10661      END-IF                                                       GBIFPGM 
10662      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMIA'                     GBIFPGM 
10663         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
10664         MOVE WS-TEMP-VALUE-LIMIT                                  GBIFPGM 
10665                            TO GCBH-SMI-BP-MAX-IP-OTH              GBIFPGM 
10666      END-IF.                                                      GBIFPGM 
10667                                                                   GBIFPGM 
10668  5060-EXIT.                                                       GBIFPGM 
10669      EXIT.                                                        GBIFPGM 
10670 /                                                                 GBIFPGM 
10671 ******************************************************************GBIFPGM 
10672 *                                                                 GBIFPGM 
10673 *    SERIOUS MENTAL ILLNESS BENEFIT PERIOD MAXIMUM - OUTPATIENT   GBIFPGM 
10674 *                                                                 GBIFPGM 
10675 ******************************************************************GBIFPGM 
10676  5070-BLD-SMI-BP-MAX-OP.                                          GBIFPGM 
10677                                                                   GBIFPGM 
10678      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMOI'                     GBIFPGM 
10679         IF WS-PROCESS-CON                                         GBIFPGM 
10680            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10681            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
10682                               TO GCBH-SMI-BP-MAX-OP-IN            GBIFPGM 
10683         END-IF                                                    GBIFPGM 
10684      END-IF                                                       GBIFPGM 
10685      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMOO'                     GBIFPGM 
10686         IF WS-PROCESS-GRP                                         GBIFPGM 
10687            PERFORM 6900-VALUE-QUALIFIER-FMT                       GBIFPGM 
10688            MOVE WS-TEMP-VALUE-LIMIT                               GBIFPGM 
10689                               TO GCBH-SMI-BP-MAX-OP-OUT           GBIFPGM 
10690         END-IF                                                    GBIFPGM 
10691      END-IF                                                       GBIFPGM 
10692      IF GAA-BAMA-ACCUMID (GAA-INDEX) = 'SMOA'                     GBIFPGM 
10693         PERFORM 6900-VALUE-QUALIFIER-FMT                          GBIFPGM 
10694         MOVE WS-TEMP-VALUE-LIMIT                                  GBIFPGM 
10695                            TO GCBH-SMI-BP-MAX-OP-OTH              GBIFPGM 
10696      END-IF.                                                      GBIFPGM 
10697                                                                   GBIFPGM 
10698  5070-EXIT.                                                       GBIFPGM 
10699      EXIT.                                                        GBIFPGM 
10700 /                                                                 GBIFPGM 
10701 ******************************************************************GBIFPGM 
10702 *                                                                 GBIFPGM 
08801 *  LOAD DEFAULT VALUES FOR MISSING ACCUM ID'S                     GBIFPGM 
08802 *                                                                 GBIFPGM 
08803 ******************************************************************GBIFPGM 
08804  6000-000-DEFAULT-VALUES.                                         GBIFPGM 
08805                                                                   GBIFPGM 
08806 *MQ 03/03/04                                                      GBIFPGM 
08807 * == DEFAULT VALUES FOR PPO/HOSPITAL ONLY ARE TREATED DIFFERENTLY GBIFPGM 
08808      EVALUATE GCG-DISCOUNT-PRODUCT-TYPE                           GBIFPGM 
08809         WHEN  'PPO'                                               GBIFPGM 
08810            PERFORM 6200-000-PROD-TYPE-PPO   THRU 6200-900-EXIT    GBIFPGM 
08811            GO TO  6000-900-EXIT                                   GBIFPGM 
08812      END-EVALUATE.                                                GBIFPGM 
08813                                                                   GBIFPGM 
08814                                                                   GBIFPGM 
08815 *JP 03/26/04 -FIX FOR HMSO ONLY CODED (NO INPAT VALUES=100%)      GBIFPGM 
08816      IF GCBH-HOSP-MEDSURG-PMT-LVL-OTH > SPACES                    GBIFPGM 
08817         CONTINUE                                                  GBIFPGM 
08818      ELSE                                                         GBIFPGM 
08819         PERFORM 6100-000-DEFAULT-VALUES  THRU 6100-900-EXIT       GBIFPGM 
08820      END-IF                                                       GBIFPGM 
08821                                                                   GBIFPGM 
08822                                                                   GBIFPGM 
08823 * == IF NO OVPI CODED, LOAD 100%.  IF NO OVPO, LOAD MSPO. ==      GBIFPGM 
08824      IF GCBH-OFF-VISIT-COPAY-IN    NOT = SPACES                   GBIFPGM 
08825         IF GCBH-OFF-VISIT-PMT-LVL-IN   = SPACES                   GBIFPGM 
08826            MOVE '      100% ' TO GCBH-OFF-VISIT-PMT-LVL-IN        GBIFPGM 
08827         END-IF                                                    GBIFPGM 
08828         IF GCBH-OFF-VISIT-PMT-LVL-OUT  = SPACES                   GBIFPGM 
08829            MOVE GCBH-MED-SURG-PMT-LVL-OUT TO                      GBIFPGM 
08830                 GCBH-OFF-VISIT-PMT-LVL-OUT                        GBIFPGM 
08831         END-IF                                                    GBIFPGM 
08832      END-IF                                                       GBIFPGM 
08833                                                                   GBIFPGM 
08834 *MQ 04/07/04                                                      GBIFPGM 
08835 * == IF NO ABCI/CBCI/SBCI, LOAD ABPI TO ABCI.  ==                 GBIFPGM 
08836      IF GCBH-IP-BCBS-MSA-PMT-LVL-IN         = SPACES              GBIFPGM 
08837        IF GCBH-IP-BC-MSA-PMT-LVL-IN         = SPACES              GBIFPGM 
08838          IF GCBH-IP-BS-MSA-PMT-LVL-IN       = SPACES              GBIFPGM 
08839            MOVE GCBH-OP-BCBS-MSA-PMT-LVL-IN   TO                  GBIFPGM 
08840                 GCBH-IP-BCBS-MSA-PMT-LVL-IN                       GBIFPGM 
08841          END-IF                                                   GBIFPGM 
08842        END-IF                                                     GBIFPGM 
08843      END-IF.                                                      GBIFPGM 
08844                                                                   GBIFPGM 
08845                                                                   GBIFPGM 
08846 *MQ 04/07/04                                                      GBIFPGM 
08847 * == IF NO ABCO/CBCO/SBCO, LOAD ABPO TO ABCO.  ==                 GBIFPGM 
08848      IF GCBH-IP-BCBS-MSA-PMT-LVL-OUT        = SPACES              GBIFPGM 
08849        IF GCBH-IP-BC-MSA-PMT-LVL-OUT        = SPACES              GBIFPGM 
08850          IF GCBH-IP-BS-MSA-PMT-LVL-OUT      = SPACES              GBIFPGM 
08851            MOVE GCBH-OP-BCBS-MSA-PMT-LVL-OUT  TO                  GBIFPGM 
08852                 GCBH-IP-BCBS-MSA-PMT-LVL-OUT                      GBIFPGM 
08853          END-IF                                                   GBIFPGM 
08854        END-IF                                                     GBIFPGM 
08855      END-IF.                                                      GBIFPGM 
08856                                                                   GBIFPGM 
08857                                                                   GBIFPGM 
08858  6000-900-EXIT.                                                   GBIFPGM 
08859      EXIT.                                                        GBIFPGM 
08860                                                                   GBIFPGM 
08861 ******************************************************************GBIFPGM 
08862 *                                                                 GBIFPGM 
08863 *  LOAD DEFAULT VALUES FOR IPHI/MSPI ACCUM ID'S                   GBIFPGM 
08864 *                                                                 GBIFPGM 
08865 ******************************************************************GBIFPGM 
08866  6100-000-DEFAULT-VALUES.                                         GBIFPGM 
08867                                                                   GBIFPGM 
08868 * == IF NO IPHI/MSPI CODED, LOAD 100% ==                          GBIFPGM 
08869      IF GCBH-HOSP-MEDSURG-PMT-LVL-IN  = SPACES  AND               GBIFPGM 
08870         GCBH-HOSP-PMT-LVL-IN      = SPACES                        GBIFPGM 
08871         MOVE '      100% '         TO GCBH-HOSP-PMT-LVL-IN        GBIFPGM 
08872      END-IF                                                       GBIFPGM 
08873      IF GCBH-HOSP-MEDSURG-PMT-LVL-IN  = SPACES  AND               GBIFPGM 
08874         GCBH-MED-SURG-PMT-LVL-IN  = SPACES                        GBIFPGM 
08875         MOVE '      100% '         TO GCBH-MED-SURG-PMT-LVL-IN    GBIFPGM 
08876      END-IF.                                                      GBIFPGM 
08877                                                                   GBIFPGM 
08878                                                                   GBIFPGM 
08879  6100-900-EXIT.                                                   GBIFPGM 
08880      EXIT.                                                        GBIFPGM 
08881                                                                   GBIFPGM 
08882 ******************************************************************GBIFPGM 
08883 *                                                                 GBIFPGM 
08884 *  LOAD DEFAULT VALUES FOR PRODUCT TYPE PPO/HOSPITAL ONLY         GBIFPGM 
08885 *                                                                 GBIFPGM 
08886 ******************************************************************GBIFPGM 
08887  6200-000-PROD-TYPE-PPO.                                          GBIFPGM 
08888                                                                   GBIFPGM 
08889 * == IF HMSI AND NO HMSO, LOAD VALUE TO MSPO. ==                  GBIFPGM 
08890      IF GCBH-HOSP-MEDSURG-PMT-LVL-IN   >  SPACES  AND             GBIFPGM 
08891         GCBH-HOSP-MEDSURG-PMT-LVL-OUT  =  SPACES                  GBIFPGM 
08892         MOVE GCBH-HOSP-MEDSURG-PMT-LVL-IN TO                      GBIFPGM 
08893              GCBH-MED-SURG-PMT-LVL-OUT                            GBIFPGM 
08894      END-IF                                                       GBIFPGM 
08895                                                                   GBIFPGM 
08896 * == IF MSPI AND NO MSPO, LOAD VALUE TO MSPO. ==                  GBIFPGM 
08897      IF GCBH-MED-SURG-PMT-LVL-IN       >  SPACES  AND             GBIFPGM 
08898         GCBH-MED-SURG-PMT-LVL-OUT      =  SPACES                  GBIFPGM 
08899         MOVE GCBH-MED-SURG-PMT-LVL-IN     TO                      GBIFPGM 
08900              GCBH-MED-SURG-PMT-LVL-OUT                            GBIFPGM 
08901      END-IF                                                       GBIFPGM 
08902                                                                   GBIFPGM 
08903 * == IF ABCI AND NO ABCO, LOAD VALUE TO SBCO. ==                  GBIFPGM 
08904      IF GCBH-IP-BCBS-MSA-PMT-LVL-IN    >  SPACES  AND             GBIFPGM 
08905         GCBH-IP-BCBS-MSA-PMT-LVL-OUT   =  SPACES                  GBIFPGM 
08906         MOVE GCBH-IP-BCBS-MSA-PMT-LVL-IN  TO                      GBIFPGM 
08907              GCBH-IP-BS-MSA-PMT-LVL-OUT                           GBIFPGM 
08908      END-IF                                                       GBIFPGM 
08909                                                                   GBIFPGM 
08910 * == IF ABPI AND NO ABPO, LOAD VALUE TO SBPO. ==                  GBIFPGM 
08911      IF GCBH-OP-BCBS-MSA-PMT-LVL-IN    >  SPACES  AND             GBIFPGM 
08912         GCBH-OP-BCBS-MSA-PMT-LVL-OUT   =  SPACES                  GBIFPGM 
08913         MOVE GCBH-OP-BCBS-MSA-PMT-LVL-IN  TO                      GBIFPGM 
08914              GCBH-OP-BS-MSA-PMT-LVL-OUT                           GBIFPGM 
08915      END-IF.                                                      GBIFPGM 
08916                                                                   GBIFPGM 
08917 * == IF SBCI AND NO SBCO, LOAD VALUE TO SBCO. ==                  GBIFPGM 
08918      IF GCBH-IP-BS-MSA-PMT-LVL-IN      >  SPACES  AND             GBIFPGM 
08919         GCBH-IP-BS-MSA-PMT-LVL-OUT     =  SPACES                  GBIFPGM 
08920         MOVE GCBH-IP-BS-MSA-PMT-LVL-IN    TO                      GBIFPGM 
08921              GCBH-IP-BS-MSA-PMT-LVL-OUT                           GBIFPGM 
08922      END-IF.                                                      GBIFPGM 
08923                                                                   GBIFPGM 
08924 * == IF SBPI AND NO SBPO, LOAD VALUE TO SBPO. ==                  GBIFPGM 
08925      IF GCBH-OP-BS-MSA-PMT-LVL-IN      >  SPACES  AND             GBIFPGM 
08926         GCBH-OP-BS-MSA-PMT-LVL-OUT     =  SPACES                  GBIFPGM 
08927         MOVE GCBH-OP-BS-MSA-PMT-LVL-IN    TO                      GBIFPGM 
08928              GCBH-OP-BS-MSA-PMT-LVL-OUT                           GBIFPGM 
08929      END-IF.                                                      GBIFPGM 
08930                                                                   GBIFPGM 
08931                                                                   GBIFPGM 
08932  6200-900-EXIT.                                                   GBIFPGM 
08933      EXIT.                                                        GBIFPGM 
08934                                                                   GBIFPGM 
08935 ******************************************************************GBIFPGM 
08936 *                                                                 GBIFPGM 
08937 *  FORMAT THE VALUE LIMIT BASED ON THE VALUE QUALIFIER.           GBIFPGM 
08938 *                                                                 GBIFPGM 
08939 ******************************************************************GBIFPGM 
08940  6900-VALUE-QUALIFIER-FMT.                                        GBIFPGM 
08941      EVALUATE GAA-BAMA-VALUE-QUALIFIER (GAA-INDEX)                GBIFPGM 
08942       WHEN '2'                                                    GBIFPGM 
08943         MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                     GBIFPGM 
08944                                    TO  WS-VALUE-LIMIT-V           GBIFPGM 
08945         MOVE WS-VALUE-LIMIT-FULL   TO  WS-VISITS-VAL              GBIFPGM 
08946         MOVE WS-VISITS     TO WS-TEMP-VALUE-LIMIT                 GBIFPGM 
08947       WHEN '3'                                                    GBIFPGM 
08948         MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                     GBIFPGM 
08949                                    TO  WS-VALUE-LIMIT-V           GBIFPGM 
08950         MOVE WS-VALUE-LIMIT-FULL   TO  WS-DAYS-VAL                GBIFPGM 
08951         MOVE WS-DAYS       TO WS-TEMP-VALUE-LIMIT                 GBIFPGM 
08952       WHEN '5'                                                    GBIFPGM 
08953         MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                     GBIFPGM 
08954                                    TO  WS-VALUE-LIMIT-S           GBIFPGM 
08955         MOVE WS-VALUE-LIMIT-S TO WS-TEMP-VALUE-LIMIT              GBIFPGM 
08956 *MQ 10/03                                                         GBIFPGM 
08957       WHEN '6'                                                    GBIFPGM 
08958         MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                     GBIFPGM 
08959                                    TO  WS-VALUE-LIMIT-V           GBIFPGM 
08960         MOVE WS-VALUE-LIMIT-FULL   TO  WS-CONFINEMENTS-VAL        GBIFPGM 
08961         MOVE WS-CONFINEMENTS       TO WS-TEMP-VALUE-LIMIT         GBIFPGM 
08962       WHEN OTHER                                                  GBIFPGM 
08963         MOVE GAA-BAMA-VALUE-LIMIT (GAA-INDEX)                     GBIFPGM 
08964                                    TO  WS-VALUE-LIMIT             GBIFPGM 
08965         MOVE WS-VALUE-LIMIT   TO WS-TEMP-VALUE-LIMIT              GBIFPGM 
08966      END-EVALUATE.                                                GBIFPGM 
08967                                                                   GBIFPGM 
08968  6900-EXIT.                                                       GBIFPGM 
08969      EXIT.                                                        GBIFPGM 
08970 /                                                                 GBIFPGM 
08971 ******************************************************************GBIFPGM 
08972 *                                                                 GBIFPGM 
08973 *  READ THE GROUP SPECIFIC RECORD.                                GBIFPGM 
08974 *                                                                 GBIFPGM 
08975 ******************************************************************GBIFPGM 
08976  8000-READ-GROUPSPC.                                              GBIFPGM 
08977                                                                   GBIFPGM 
08978      COMPUTE  WS-IO-PARM-GROUPSPC-LEN      =                      GBIFPGM 
08979               GC-GCIOPARM-LEN              +                      GBIFPGM 
08980               GC-GCGRPSPC-FIXED-LEN        +                      GBIFPGM 
08981             ( GC-GCGRPSPC-VARY-MAX-OCUR    *                      GBIFPGM 
08982               GC-GCGRPSPC-VARY-LEN ).                             GBIFPGM 
08983                                                                   GBIFPGM 
08984      EXEC CICS GETMAIN  SET (IO-PARM-GROUPSPC-AREA-1)             GBIFPGM 
08985                         INITIMG(WS-HEX-00)                        GBIFPGM 
08986                         LENGTH (WS-IO-PARM-GROUPSPC-LEN)          GBIFPGM 
08987                         END-EXEC.                                 GBIFPGM 
08988                                                                   GBIFPGM 
08989      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GBIFPGM 
08990                                   TO GCG-COUNT-TAB-PROVN-POINTERS.GBIFPGM 
08991      MOVE 'RD '                   TO GCIO-FILE-ACCESS-CODE.       GBIFPGM 
08992      MOVE 'GCGRPSPC'              TO GCIO-FILE-DDNAME.            GBIFPGM 
08993      MOVE '1'                     TO GCIO-IO-AREA-TO-USE.         GBIFPGM 
08994      MOVE GCG-GRP-SPECIF-ID       TO GCIO-FILE-KEY.               GBIFPGM 
08995                                                                   GBIFPGM 
08996      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         GBIFPGM 
08997                       COMMAREA(IO-PARM-GROUPSPC-AREA-1)           GBIFPGM 
08998                       LENGTH  (WS-IO-PARM-GROUPSPC-LEN)           GBIFPGM 
08999                       END-EXEC.                                   GBIFPGM 
09000                                                                   GBIFPGM 
09001  8000-EXIT.                                                       GBIFPGM 
09002      EXIT.                                                        GBIFPGM 
09003 /                                                                 GBIFPGM 
09004 ******************************************************************GBIFPGM 
09005 *                                                                 GBIFPGM 
09006 *  READ THE CONTRACT RECORD.                                      GBIFPGM 
09007 *                                                                 GBIFPGM 
09008 ******************************************************************GBIFPGM 
09009  8100-READ-CONTRACT.                                              GBIFPGM 
09010                                                                   GBIFPGM 
09011      COMPUTE  WS-IO-PARM-CONTRACT-LEN      =                      GBIFPGM 
09012               GC-GCIOPARM-LEN              +                      GBIFPGM 
09013               GC-GCCONTR-FIXED-LEN         +                      GBIFPGM 
09014             ( GC-GCCONTR-VARY-MAX-OCUR     *                      GBIFPGM 
09015               GC-GCCONTR-VARY-LEN ).                              GBIFPGM 
09016                                                                   GBIFPGM 
09017      EXEC CICS GETMAIN  SET (IO-PARM-CONTRACT-AREA-1)             GBIFPGM 
09018                         INITIMG(WS-HEX-00)                        GBIFPGM 
09019                         LENGTH (WS-IO-PARM-CONTRACT-LEN)          GBIFPGM 
09020                         END-EXEC.                                 GBIFPGM 
09021                                                                   GBIFPGM 
09022      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GBIFPGM 
09023                                   TO GCT-COUNT-BEN-PROVN-POINTERS.GBIFPGM 
09024      MOVE 'RD '                   TO GCIO3-FILE-ACCESS-CODE.      GBIFPGM 
09025      MOVE 'GCCONTR '              TO GCIO3-FILE-DDNAME.           GBIFPGM 
09026      MOVE '1'                     TO GCIO3-IO-AREA-TO-USE.        GBIFPGM 
09027      MOVE GCT-CONTRACT-ID         TO GCIO3-FILE-KEY.              GBIFPGM 
09028                                                                   GBIFPGM 
09029      EXEC CICS LINK   PROGRAM ('GCIOPGM')                         GBIFPGM 
09030                       COMMAREA(IO-PARM-CONTRACT-AREA-1)           GBIFPGM 
09031                       LENGTH  (WS-IO-PARM-CONTRACT-LEN)           GBIFPGM 
09032                       END-EXEC.                                   GBIFPGM 
09033                                                                   GBIFPGM 
09034  8100-EXIT.                                                       GBIFPGM 
09035      EXIT.                                                        GBIFPGM 
09036 /                                                                 GBIFPGM 
09037 ******************************************************************GBIFPGM 
09038 *                                                                 GBIFPGM 
09039 *  READ THE TABULAR RECORD.                                       GBIFPGM 
09040 *                                                                 GBIFPGM 
09041 ******************************************************************GBIFPGM 
09042  8200-READ-TABULAR.                                               GBIFPGM 
09043                                                                   GBIFPGM 
09044      COMPUTE   WS-IO-PARM-TABULAR-LEN     =                       GBIFPGM 
09045                GC-GCIOPARM-LEN            +                       GBIFPGM 
09046                GC-GCTABULR-ABM-FIXED-LEN  +                       GBIFPGM 
09047               (GC-GCTABULR-ABM-VARY-LEN   *                       GBIFPGM 
09048                GC-GCTABULR-ABM-VARY-MAX-OCUR).                    GBIFPGM 
09049                                                                   GBIFPGM 
09050      EXEC CICS GETMAIN                                            GBIFPGM 
09051                SET(IO-PARM-TABULAR-AREA-1)                        GBIFPGM 
09052                INITIMG(WS-HEX-00)                                 GBIFPGM 
09053                LENGTH(WS-IO-PARM-TABULAR-LEN)                     GBIFPGM 
09054      END-EXEC.                                                    GBIFPGM 
09055                                                                   GBIFPGM 
09056      MOVE 'RD '                    TO GCIO5-FILE-ACCESS-CODE.     GBIFPGM 
09057      MOVE 'GCTABULR'               TO GCIO5-FILE-DDNAME.          GBIFPGM 
09058      MOVE '1'                      TO GCIO5-IO-AREA-TO-USE.       GBIFPGM 
09059      MOVE GAA-TABULAR-PROVISION-ID TO GCIO5-FILE-KEY.             GBIFPGM 
09060                                                                   GBIFPGM 
09061      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GBIFPGM 
09062        TO GAA-ENTRY-COUNT.                                        GBIFPGM 
09063                                                                   GBIFPGM 
09064      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GBIFPGM 
09065         COMMAREA(IO-PARM-TABULAR-AREA-1)                          GBIFPGM 
09066         LENGTH(WS-IO-PARM-TABULAR-LEN) END-EXEC.                  GBIFPGM 
09067                                                                   GBIFPGM 
09068      IF NOT GCIO5-GOOD-RETURN                                     GBIFPGM 
09069         MOVE '1BF3'  TO  WS-ABEND-CODE                            GBIFPGM 
09070         PERFORM 9999-ABEND.                                       GBIFPGM 
09071                                                                   GBIFPGM 
09072  8200-EXIT.                                                       GBIFPGM 
09073      EXIT.                                                        GBIFPGM 
09074 /                                                                 GBIFPGM 
09075 ******************************************************************GBIFPGM 
09076 *                                                                 GBIFPGM 
09077 *  CONVERT JULIAN DATE INTO GREGORIAN FORMAT.                     GBIFPGM 
09078 *                                                                 GBIFPGM 
09079 ******************************************************************GBIFPGM 
09080  9200-JUL-TO-GREG-DATE.                                           GBIFPGM 
09081                                                                   GBIFPGM 
09082      MOVE '9200' TO WS-PARA-ID.                                   GBIFPGM 
09083                                                                   GBIFPGM 
09084      MOVE 'CNV' TO  HGADATE-FUNC.                                 GBIFPGM 
09085      MOVE 'J'   TO  HGADATE-FORM1.                                GBIFPGM 
09086      MOVE 'M'   TO  HGADATE-FORM2.                                GBIFPGM 
09087      MOVE ZEROS TO  HGADATE-RETURN                                GBIFPGM 
09088                     HGADATE-AMOUNT.                               GBIFPGM 
09089      EXEC CICS LINK PROGRAM ('HGADATES')                          GBIFPGM 
09090                     COMMAREA(HGADATES-COMMAREA)                   GBIFPGM 
09091                     LENGTH  (24)                                  GBIFPGM 
09092                     END-EXEC.                                     GBIFPGM 
09093                                                                   GBIFPGM 
09094  9200-EXIT.                                                       GBIFPGM 
09095      EXIT.                                                        GBIFPGM 
09096 /                                                                 GBIFPGM 
09097 ******************************************************************GBIFPGM 
09098 * THIS CATCHES THE SITUATION IN WHICH PROGRAM EXECUTION SEQUENCE *GBIFPGM 
09099 * 'FALLS THROUGH' THE BOTTOM OF THE PROGRAM.                     *GBIFPGM 
09100 ******************************************************************GBIFPGM 
09101  9999-FALL-THRU-TRAP.                                             GBIFPGM 
09102      MOVE 'L104' TO WS-ABEND-CODE.                                GBIFPGM 
09103                                                                   GBIFPGM 
09104 /                                                                 GBIFPGM 
09105 ******************************************************************GBIFPGM 
09106 * THIS ERROR CAN BE INVOKED BY A NUMBER OF DIFFERENT REQUESTS    *GBIFPGM 
09107 * THE PROGRAMMER SHOULD CHECK THE WS-PARA-ID FIELD IN THE        *GBIFPGM 
09108 * DUMP TO DETERMINE WHAT CODE CAUSED THIS ABEND.                 *GBIFPGM 
09109 ******************************************************************GBIFPGM 
09110  9999-ABEND.                                                      GBIFPGM 
09111                                                                   GBIFPGM 
09112      MOVE '9999' TO WS-PARA-ID.                                   GBIFPGM 
09113                                                                   GBIFPGM 
09114      EXEC CICS ABEND  ABCODE(WS-ABEND-CODE)                       GBIFPGM 
09115                       END-EXEC.                                   GBIFPGM 
09116                                                                   GBIFPGM 
09117  9999-EXIT.                                                       GBIFPGM 
09118      EXIT.                                                        GBIFPGM 
