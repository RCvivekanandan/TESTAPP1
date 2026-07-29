00001  TITLE 'NA/ES- INTERFACE PROGRAM'.                                12/13/05
00002  IDENTIFICATION DIVISION.                                         ELXPMCIF
00003                                                                      LV004
00004  PROGRAM-ID.         ELXPMCIF.                                    ELXPMCIF
00005                                                                   ELXPMCIF
00006  AUTHOR.             ANNE KEFFER-KING.                            ELXPMCIF
00007                      RESTRUCTURED BY RJL.                         ELXPMCIF
00008                                                                   ELXPMCIF
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCIF
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCIF
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCIF
00012                      233 N. MICHIGAN AVE                          ELXPMCIF
00013                      CHICAGO, ILLINOIS 60601                      ELXPMCIF
00014                                                                   ELXPMCIF
00015  DATE-WRITTEN.       15-JUL-1992.                                 ELXPMCIF
00016                                                                   ELXPMCIF
00017  DATE-COMPILED.                                                   ELXPMCIF
00018                                                                   ELXPMCIF
00019  SECURITY.           COPYRIGHT 1992,                              ELXPMCIF
00020                      HEALTH CARE SERVICE CORPORATION              ELXPMCIF
00021                                                                   ELXPMCIF
00022  ENVIRONMENT DIVISION.                                            ELXPMCIF
00023                                                                   ELXPMCIF
00024  CONFIGURATION SECTION.                                           ELXPMCIF
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELXPMCIF
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELXPMCIF
00027      EJECT                                                        ELXPMCIF
00028 ******************************************************************ELXPMCIF
00029 **  !!!!! THIS PGM SHOULD ALWAYS BE COMPILED LAST WHEN CHANGES   *ELXPMCIF
00030 ****!!!!! MADE TO ANY PGM IN THE PMCIF FAMILY OF PGMS!!!!      ***ELXPMCIF
00031 ******************************************************************ELXPMCIF
00032 *    PROGRAM:    ELXPMCIF                                        *ELXPMCIF
00033 *    DATE:       15-JUL-1992                                     *ELXPMCIF
00034 *    AUTHOR:     ANNE KEFFER-KING                                *ELXPMCIF
00035 *    FUNCTION:                                                   *ELXPMCIF
00036 *      ESTABLISH THE ELS ENVIRONMENT AND INCORPORATE THE         *ELXPMCIF
00037 *      PMCI COMMAREA PASSED IN BY THE CALLING PROGRAM.           *ELXPMCIF
00038 *                                                                *ELXPMCIF
00039 *      - INITIALIZE THE PMCI RESULTS                             *ELXPMCIF
00040 *      - INITIALIZE ANY ADDITIONAL ELS CONTROL AREAS             *ELXPMCIF
00041 *      - INITIALIZE AND ADDRESS CIA                              *ELXPMCIF
00042 *      - INITIALIZE AND ADDRESS COMMAREA                         *ELXPMCIF
00043 *      - ADDRESS SELECTOR STATUS CONTROL BLOCKS                  *ELXPMCIF
00044 *      - ADDRESS PMCI VIA SMA                                    *ELXPMCIF
00045 *      - CALL ELXPMCGC (MODULE DETERMINES SPECIFIC GROUP/        *ELXPMCIF
00046 *        CONTRACT                                                *ELXPMCIF
00047 *      - IF RETURN FOR ELXPMCGC IS OK A CALL WILL BE MADE TO     *ELXPMCIF
00048 *        THE DERIVATION SUBROUTINES (ELXPMCCP ELXPMCGA ELXPMCBP  *ELXPMCIF
00049 *        AND ELXPMCAC)                                           *ELXPMCIF
00050 *                                                                *ELXPMCIF
00051 ******************************************************************ELXPMCIF
00052 * AKK 12/06/05 REGEN                                             *ELXPMCIF
00053 *                      MAINTENANCE HISTORY                       *ELXPMCIF
00054 *                                                                *ELXPMCIF
00055 * MOD      DATE     BY                   ACTION                  *ELXPMCIF
00056 * ----- ----------- --- ---------------------------------------- *ELXPMCIF
00057 * 01.00 15-JUL-1992 AKK CREATED                                  *ELXPMCIF
00058 *                                                                *ELXPMCIF
00059 *                                                                *ELXPMCIF
00060 * 01.01 09-NOV-1992 AKK CHANGED INITIALIZATION OF PMCI FIELDS    *ELXPMCIF
00061 *                       DUE TO REQUEST TO MOVE WHAT WE HAVE AS   *ELXPMCIF
00062 *                       OF TODAY'S DATE TO PRODUCTION.  THE IN-  *ELXPMCIF
00063 *                       ITIAL VALUES WILL BE RETURNED TO NORMAL  *ELXPMCIF
00064 *                       WHEN ALL PHASES OF THE PROJECT ARE COM-  *ELXPMCIF
00065 *                       PLETE.                                   *ELXPMCIF
00066 *                                                                *ELXPMCIF
00067 * 01.02 07-DEC-1992 AKK ADDED CALL TO ELXPMCAC.                   ELXPMCIF
00068 * 02.00 05-AUG-1993 BAK RESTORE PRODUCTION VERSION FOR PANLIB     ELXPMCIF
00069 * 03.00 09-JAN-1995 RGO ADDED CODE TO PROCESS SUPPLEMENTAL MEDICARELXPMCIF
00070 *                       PRODUCTS.                                 ELXPMCIF
00071 * 4/25/95  RGO. MEDICARE SUPPLEMENT, PHASE 2. REMOVE TEMP DEFAULT ELXPMCIF
00072 *                VALUES TO HOME-VISIT,MED-SUPPLY, PROSTHETICS     ELXPMCIF
00073 *                AND BCBS-DEDUCTIBLE.                             ELXPMCIF
00074 * 12/08/98 AKK  ADD CODE TO SET ALL COPAYS TO CALL WHEN ACP IS    ELXPMCIF
00075 *               DETECTED.                                         ELXPMCIF
00076 *                                                                 ELXPMCIF
00077 * 08/15/00  JP  COMMENTED OUT ACP CODING (PARA 8000).             ELXPMCIF
00078 *                                                                 ELXPMCIF
00079 * 04/01/03 AKK  MORE CHANGES DUE TO ENDEVOR.                      ELXPMCIF
00080 *                                                                 ELXPMCIF
00081 * 06/22/03 AKK  RGEN DUE TO FILE EXPANSION .                      ELXPMCIF
00082 *                                                                 ELXPMCIF
00083 * 06/23/03 AKK  RGEN DUE TO CHANGES IN OTHER PROGRAMS FOR FILE    ELXPMCIF
00084 *               EXPANSION.                                        ELXPMCIF
00085 * 06/24/03 AKK  REGEN DUE TO CHANGES IN CALLED PGMS.              ELXPMCIF
00086 * 12/01/03 AKK  MOVING TO TEST TO TEST LINK CARD.                 ELXPMCIF
00087 * 01/09/04 AKK  S0C7 INTERTEST                                    ELXPMCIF
00088 * 02.01 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCIF
00089 *                                                                *ELXPMCIF
00090 * 03.0  25-APR-2005 AKK RECOMPILE FOR INTERTEST                   ELXPMCIF
00091 *                                                                *ELXPMCIF
00092 * 03.1  27-APR-2005 AKK ADD FREEMAINS FOR CIA AREAS 9999- PARA.   ELXPMCIF
00093 *                                                                *ELXPMCIF
00094 * 03.2  13-JUL-2005 AKK REGEN FOR INTERTEST.                      ELXPMCIF
00095 *                                                                *ELXPMCIF
00096 * 04.0  06-DEC-2005 AKK REGEN'D DUE TO ELSCIAC CHANGE TO FIX      ELXPMCIF
00097 *                       STORAGE VIOLATION                       * ELXPMCIF
00095 *                                                                *ELXPMCIF
SK0724* P56703 05/10/2024 SK  RECOMPILE - ACCUM TABULAR COPYBOOKS      *ELGABMCC
SK0724*                                   EXPANSION                    *ELGABMCC
00056 *                                                                *ELGABMCC
00098 ******************************************************************ELXPMCIF
00099      EJECT                                                        ELXPMCIF
00100  DATA DIVISION.                                                   ELXPMCIF
00101  WORKING-STORAGE SECTION.                                         ELXPMCIF
00102  01  WS-HDG                      PICTURE X(32)                    ELXPMCIF
00103      VALUE '******* WS STARTS HERE *******'.                      ELXPMCIF
00104                                                                   ELXPMCIF
00105 * TEXAS REGIONS FOR PACKAGE CODE CHECK                            ELXPMCIF
00106  01  WS-APPLID.                                                   ELXPMCIF
00107      02 FILLER                   PIC X(03).                       ELXPMCIF
00108      02 FILLER                   PIC X(04).                       ELXPMCIF
00109         88 TEXAS-REGION          VALUES                           ELXPMCIF
00110         'XAI1' 'XAI2' 'XAB1' 'XAB2' 'XAB3' 'XAB4' 'XAB5'          ELXPMCIF
00111         'XAB6' 'XAB7' 'XAB8' 'XAB9' 'XAS1' 'XAS2' 'XFB1'          ELXPMCIF
00112         'XFB2' 'XF01'.                                            ELXPMCIF
00113                                                                   ELXPMCIF
00114  01  WS-GROUPS-TO-SKIP.                                           ELXPMCIF
00115      02 WS-CHANGE-TX-GROUP-CHECK PIC X(09).                       ELXPMCIF
00116         88  WS-CHANGE-TX-PKG-CODE-GROUP                           ELXPMCIF
00117              VALUES '0000FEPTX' '000051200' '000051201'           ELXPMCIF
00118                     '000051300' '000051301' '000000600'           ELXPMCIF
00119                     '000061100' '000061500' '000061600'           ELXPMCIF
00120                     '000071100'.                                  ELXPMCIF
00121                                                                   ELXPMCIF
00122 * -- ELS ENVIRONMENTG COMMAREA - REPLACES NA/ES PMCI              ELXPMCIF
00123 * -- AREA AS COMMAREA. PMCI IS ATTACHED VIA STORAGE MANAGER.      ELXPMCIF
00124                                                                   ELXPMCIF
00125  01  WS-COMMAREA.                                                 ELXPMCIF
00126      05  WS-CIA-PTR              POINTER.                         ELXPMCIF
00127 /ELS COMMON INTERFACE AREA                                        ELXPMCIF
00128  COPY ELSCIAC.                                                    ELXPMCIF
00129 /PMCI INTERFACE INTERNAL DATA SAVE BLOCK                          ELXPMCIF
00130  COPY ELSPMCID.                                                   ELXPMCIF
00131                                                                   ELXPMCIF
00132  01 ABM-KEY.                                                      ELXPMCIF
00133      05  WS-ABM-TAB-NAME   PIC X(6) VALUE '#ABM  '.               ELXPMCIF
00134      05  WS-ABM-SLOT       COMP-3 PIC S9(7).                      ELXPMCIF
00135                                                                   ELXPMCIF
00136  01 SUB1   COMP-3  PIC S9(2) VALUE 0.                             ELXPMCIF
00137  01 WS-IO-PARM-ALL-LVL-LEN      PIC S9(4) COMP VALUE ZEROS.       ELXPMCIF
00138  01 WS-HEX-00                   PIC X.                            ELXPMCIF
00139                                                                   ELXPMCIF
00140  01 WS-GCPS-MISC-LENGTHS.                                         ELXPMCIF
00141  COPY GCCDRLEN.                                                   ELXPMCIF
00142                                                                   ELXPMCIF
00143 /ABM TABULAR I/O MODULE AND AREA                                  ELXPMCIF
00144 /******  LINKAGE SECTION                                          ELXPMCIF
00145  01 ABM-POINTER    POINTER VALUE NULL.                            ELXPMCIF
00146  LINKAGE SECTION.                                                 ELXPMCIF
00147                                                                   ELXPMCIF
00148  01  DFHCOMMAREA.                                                 ELXPMCIF
00149  COPY ELSCOMMC.                                                   ELXPMCIF
00150 /SELECTOR STATUS CONTROL BLOCK (MAY NOT BE NEEDED)                ELXPMCIF
00151  COPY ELSSSCBC.                                                   ELXPMCIF
00152 /PMCI COMMAREA PASSED FROM PMCI TRANSACTION                       ELXPMCIF
00153  01 PMCI-COMM-AREA.                                               ELXPMCIF
00154  COPY PMCCOMM.                                                    ELXPMCIF
00155 /                                                                 ELXPMCIF
00156  01 IO-PARM-ABM.                                                  ELXPMCIF
00157  COPY GCIOPRM1.                                                   ELXPMCIF
00158  01 ABM-RECORD.                                                   ELXPMCIF
00159  COPY GCTABMC.                                                    ELXPMCIF
00160 /***********************************************************      ELXPMCIF
00161 *                                                          *      ELXPMCIF
00162 *                    PROCEDURE DIVISION                    *      ELXPMCIF
00163 *                                                          *      ELXPMCIF
00164 ************************************************************      ELXPMCIF
00165                                                                   ELXPMCIF
00166  PROCEDURE DIVISION.                                              ELXPMCIF
00167                                                                   ELXPMCIF
00168 ************************************************************      ELXPMCIF
00169 *                                                          *      ELXPMCIF
00170 *    ELXPMCIF MAINLINE                                     *      ELXPMCIF
00171 *                                                          *      ELXPMCIF
00172 ************************************************************      ELXPMCIF
00173                                                                   ELXPMCIF
00174  0000-ELXPMCIF-MAINLINE.                                          ELXPMCIF
00175      IF EIBCALEN = ZERO                                           ELXPMCIF
00176          CONTINUE                                                 ELXPMCIF
00177      ELSE                                                         ELXPMCIF
00178          PERFORM 1000-INITIALIZE-NAES-INTRFC                      ELXPMCIF
00179          PERFORM 2000-INITIALIZE-PMCI-VALUES                      ELXPMCIF
00180          PERFORM 3000-OBTN-GRP-CNTRCT-RCRDS                       ELXPMCIF
00181          IF PMCI-BC-SUCCESSFUL AND                                ELXPMCIF
00182             PMCI-SUPP-MED = 'Y'                                   ELXPMCIF
00183             PERFORM 5000-GET-SUPP-MED-INFO THRU 5000-EXIT.        ELXPMCIF
00184          IF PMCI-BC-SUCCESSFUL                                    ELXPMCIF
00185             PERFORM 4000-CALL-SUB-PROGRAMS                        ELXPMCIF
00186          END-IF.                                                  ELXPMCIF
00187 *        PERFORM 8000-DETERMINE-ACP-VALUES                        ELXPMCIF
00188          PERFORM 9999-DO-FREEMAINS.                               ELXPMCIF
00189                                                                   ELXPMCIF
00190      GOBACK.                                                      ELXPMCIF
00191 /***********************************************************      ELXPMCIF
00192 *                                                          *      ELXPMCIF
00193 *    INITIALIZE THE NA/ES INTERFACE                        *      ELXPMCIF
00194 *                                                          *      ELXPMCIF
00195 ************************************************************      ELXPMCIF
00196  1000-INITIALIZE-NAES-INTRFC.                                     ELXPMCIF
00197                                                                   ELXPMCIF
00198 * -- ESTABLISH ELS STORAGE MANAGEMENT ENVIRONMENT                 ELXPMCIF
00199                                                                   ELXPMCIF
00200 * -- SAVE INCOMING COMMAREA ADDRESS IN SMA                        ELXPMCIF
00201      SET CIA-PMCCOMM-PTR TO ADDRESS OF DFHCOMMAREA.               ELXPMCIF
00202                                                                   ELXPMCIF
00203 * -- ESTABLISH ELS COMMAREA, CIA AND SMA FOR REMAINING PROGRAMS   ELXPMCIF
00204      CALL 'ELUADDRS' USING WS-COMMAREA                            ELXPMCIF
00205                            ADDRESS OF DFHCOMMAREA.                ELXPMCIF
00206      CALL 'ELUADDRS' USING CIA-ELS-COMMON-INTERFACE-AREA          ELXPMCIF
00207                            ECA-CIA-PTR.                           ELXPMCIF
00208      SET CIA-ELSCIA-PTR TO ECA-CIA-PTR.                           ELXPMCIF
00209      MOVE +4 TO EIBCALEN.                                         ELXPMCIF
00210      CALL 'ELUADDRS' USING CIA-STG-MGT                            ELXPMCIF
00211                            CIA-ELSSMA-PTR.                        ELXPMCIF
00212                                                                   ELXPMCIF
00213 * -- INITIALIZE ELS TEMPORARY STORAGE AREAS                       ELXPMCIF
00214      SET CIA-STG-INITIALIZE TO TRUE.                              ELXPMCIF
00215      CALL 'ELUSTGMG' USING DFHEIBLK                               ELXPMCIF
00216                            DFHCOMMAREA.                           ELXPMCIF
00217                                                                   ELXPMCIF
00218 * -- PUT ADDRESS OF COMMON INTERMEDIATE AREA IN SMA               ELXPMCIF
00219      CALL 'ELUADDRS' USING NAES-INTERMEDIATE-DATA                 ELXPMCIF
00220                            CIA-ELSPMCID-PTR.                      ELXPMCIF
00221                                                                   ELXPMCIF
00222 * -- ESTABLISH ADDRESSABILTY TO PMCI                              ELXPMCIF
00223      SET ADDRESS OF PMCI-COMM-AREA TO CIA-PMCCOMM-PTR.            ELXPMCIF
00224                                                                   ELXPMCIF
00225 * -- DETERMINE NEED TO SET PKG CODE TO ZERO FOR SPECIAL TX        ELXPMCIF
00226 *-- GROUPS                                                        ELXPMCIF
00227      EXEC CICS ASSIGN APPLID (WS-APPLID) END-EXEC.                ELXPMCIF
00228      MOVE PMCI-GROUP-NBR TO WS-CHANGE-TX-GROUP-CHECK.             ELXPMCIF
00229      IF TEXAS-REGION AND WS-CHANGE-TX-PKG-CODE-GROUP              ELXPMCIF
00230           MOVE 000 TO PMCI-PACKAGE-CODE                           ELXPMCIF
00231      END-IF.                                                      ELXPMCIF
00232                                                                   ELXPMCIF
00233 /***********************************************************      ELXPMCIF
00234 *                                                          *      ELXPMCIF
00235 *    INITILIZE PMCI VALUES                                 *      ELXPMCIF
00236 *                                                          *      ELXPMCIF
00237 ************************************************************      ELXPMCIF
00238                                                                   ELXPMCIF
00239  2000-INITIALIZE-PMCI-VALUES.                                     ELXPMCIF
00240      INITIALIZE PMCI-CONTRACT-INFO.                               ELXPMCIF
00241      SET AMBULANCE-CALL                                           ELXPMCIF
00242          CHC-CALL                                                 ELXPMCIF
00243          COB-CALL                                                 ELXPMCIF
00244          PMCI-COINS-CALL                                          ELXPMCIF
00245          DRB-CALL                                                 ELXPMCIF
00246          PMCI-MAX-COMBO-NONE                                      ELXPMCIF
00247          SUB-ABUSE-ALC-CALL                                       ELXPMCIF
00248          SUB-ABUSE-DRG-CALL                                       ELXPMCIF
00249          PMCI-AL-CALL                                             ELXPMCIF
00250          PMCI-MD-CALL                                             ELXPMCIF
00251          PSY-CALL                                                 ELXPMCIF
00252          DNPD-CALL                                                ELXPMCIF
00253          DNPN-CALL                                                ELXPMCIF
00254          PMCI-IQ-CALL                                             ELXPMCIF
00255          LFMD-NO-COVERAGE                                         ELXPMCIF
00256          PMCI-MM-CALL                                             ELXPMCIF
00257          PMCI-LM-CALL                                             ELXPMCIF
00258          MNDD-CALL                                                ELXPMCIF
00259          MNDT-CALL                                                ELXPMCIF
00260          PMCI-DEDI-CALL                                           ELXPMCIF
00261          PMCI-DEDF-CALL                                           ELXPMCIF
00262          DME-CALL                                                 ELXPMCIF
00263          DME-MD-CERT-CALL                                         ELXPMCIF
00264          EAC-CALL                                                 ELXPMCIF
00265          EMC-CALL                                                 ELXPMCIF
00266          ELT-CALL                                                 ELXPMCIF
00267          HOSPICE-CALL                                             ELXPMCIF
00268          HOSP-LFT-MAX-CALL                                        ELXPMCIF
00269          HOTS-CALL                                                ELXPMCIF
00270          HOTS-LFT-MAX-CALL                                        ELXPMCIF
00271          PMCI-LAB-CALL                                            ELXPMCIF
00272          PMCI-MED-RES-DAYS-CALL                                   ELXPMCIF
00273          PMCI-PPO-CALL                                            ELXPMCIF
00274          PMCI-ASOP-CALL                                           ELXPMCIF
00275          PMCI-MASOP-CALL                                          ELXPMCIF
00276          PMCI-MOPS-CALL                                           ELXPMCIF
00277          PMCI-MSA-CALL                                            ELXPMCIF
00278          MSAD-CALL                                                ELXPMCIF
00279          OB-NORM-CALL                                             ELXPMCIF
00280          OB-COMP-CALL                                             ELXPMCIF
00281          PMCI-PHY-THRPY-CALL                                      ELXPMCIF
00282          PMCI-OCC-THRPY-CALL                                      ELXPMCIF
00283          PMCI-OFF-VISITS-CALL                                     ELXPMCIF
00284          OPX-CALL                                                 ELXPMCIF
00285          PMCI-PDN-CALL                                            ELXPMCIF
00286          PRDN-CALL                                                ELXPMCIF
00287          PRE-CALL                                                 ELXPMCIF
00288          PMCI-PRE-EXIST-CALL                                      ELXPMCIF
00289          PRRT-CALL                                                ELXPMCIF
00290          SPT-CALL                                                 ELXPMCIF
00291          TIMELY-FILING-CALL                                       ELXPMCIF
00292          WEEKEND-ADMIN-CALL                                       ELXPMCIF
00293          PMCI-PPO-PROV-CALL                                       ELXPMCIF
00294          XRAY-CALL              TO TRUE.                          ELXPMCIF
00295      MOVE 'N' TO PMCI-SUPP-MED                                    ELXPMCIF
00296                  PMCI-PARTA-DEDUCT                                ELXPMCIF
00297                  PMCI-PARTA-COIN                                  ELXPMCIF
00298                  PMCI-PARTA-LIFE                                  ELXPMCIF
00299                  PMCI-PARTA-SNF-COIN                              ELXPMCIF
00300                  PMCI-PARTB-BCBS-DED                              ELXPMCIF
00301                  PMCI-PARTB-ANN-DED.                              ELXPMCIF
00302      MOVE ZERO TO PMCI-ABM-SLOT.                                  ELXPMCIF
00303 /***********************************************************      ELXPMCIF
00304 *                                                          *      ELXPMCIF
00305 *    INITILIZE PMCI VALUES                                 *      ELXPMCIF
00306 *                                                          *      ELXPMCIF
00307 ************************************************************      ELXPMCIF
00308                                                                   ELXPMCIF
00309  3000-OBTN-GRP-CNTRCT-RCRDS.                                      ELXPMCIF
00310                                                                   ELXPMCIF
00311 * -- ASSUME THAT SUBROUTINE WILL FAIL, JUST IN CASE               ELXPMCIF
00312 * -- IT IS UNABLE TO ESTABLISH STORAGE ENVIRONMENT                ELXPMCIF
00313      SET PMCI-BC-INTERNAL-ERROR TO TRUE                           ELXPMCIF
00314      MOVE +0999 TO PMCI-BLUE-CHIP-ERROR-CODE                      ELXPMCIF
00315                                                                   ELXPMCIF
00316      CALL 'ELXPMCGC' USING DFHEIBLK DFHCOMMAREA.                  ELXPMCIF
00317 /***********************************************************      ELXPMCIF
00318 *                                                          *      ELXPMCIF
00319 *    INITILIZE PMCI VALUES                                 *      ELXPMCIF
00320 *                                                          *      ELXPMCIF
00321 ************************************************************      ELXPMCIF
00322                                                                   ELXPMCIF
00323  4000-CALL-SUB-PROGRAMS.                                          ELXPMCIF
00324                                                                   ELXPMCIF
00325 * -- SET ERROR INDICATOR FOR UNABLE TO ESTABLISH ENVIRONMENT      ELXPMCIF
00326      SET PMCI-BC-INTERNAL-ERROR TO TRUE.                          ELXPMCIF
00327      MOVE +1000 TO PMCI-BLUE-CHIP-ERROR-CODE.                     ELXPMCIF
00328                                                                   ELXPMCIF
00329 * -- EXTRACT COST CONTAINMENT INFORMATION                         ELXPMCIF
00330      CALL 'ELXPMCCP' USING DFHEIBLK DFHCOMMAREA.                  ELXPMCIF
00331                                                                   ELXPMCIF
00332      IF PMCI-BC-SUCCESSFUL                                        ELXPMCIF
00333      THEN                                                         ELXPMCIF
00334 *    -- SET ERROR INDICATOR FOR UNABLE TO ESTABLISH ENVIRONMENT   ELXPMCIF
00335         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCIF
00336         MOVE +2000 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCIF
00337 *    -- EXTRACT GENERAL CONTRACT ADMINISTRATION RULES             ELXPMCIF
00338         CALL 'ELXPMCGA' USING DFHEIBLK DFHCOMMAREA                ELXPMCIF
00339         IF PMCI-BC-SUCCESSFUL                                     ELXPMCIF
00340         THEN                                                      ELXPMCIF
00341 *       -- SET ERROR INDICATOR FOR UNABLE TO ESTABLISH ENVIRONMENTELXPMCIF
00342            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCIF
00343            MOVE +3000 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCIF
00344 *       -- EXTRACT BENEFIT PROVISION RULES                        ELXPMCIF
00345            CALL 'ELXPMCBP' USING DFHEIBLK DFHCOMMAREA             ELXPMCIF
00346         IF PMCI-BC-SUCCESSFUL                                     ELXPMCIF
00347         THEN                                                      ELXPMCIF
00348 *       -- SET ERROR INDICATOR FOR UNABLE TO ESTABLISH ENVIRONMENTELXPMCIF
00349            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCIF
00350            MOVE +4000 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCIF
00351 *       -- EXTRACT ACCUMUALTOR RULES                              ELXPMCIF
00352            CALL 'ELXPMCAC' USING DFHEIBLK DFHCOMMAREA.            ELXPMCIF
00353 /***********************************************************      ELXPMCIF
00354 * 5000-                                                    *      ELXPMCIF
00355 *    GET SUPPLEMENTAL MEDICARE INFORMATION                 *      ELXPMCIF
00356 * 1. GET THE ABM TABULAR RECORD.                           *      ELXPMCIF
00357 * 2. LOOP THRU THE INTERNAL DESCRIPTORS, SEARCHING FOR     *      ELXPMCIF
00358 *    EACH OF 4 INTERNAL DESCRIPTORS.                       *      ELXPMCIF
00359 * 3. CHECK EACH OF THE 4 FOR IBGR SLOT NUMBER, AND SET     *      ELXPMCIF
00360 *    THE CORRESPONDING FLAG.                               *      ELXPMCIF
00361 * 4. IF ANY CICS ERROR OCCURS, ALL FLAGS WILL BE SET TO    *      ELXPMCIF
00362 *    CALL, AND PROCESSING WILL CONTINUE.                   *      ELXPMCIF
00363 ************************************************************      ELXPMCIF
00364 *                                                                 ELXPMCIF
00365  5000-GET-SUPP-MED-INFO.                                          ELXPMCIF
00366      EXEC CICS HANDLE CONDITION                                   ELXPMCIF
00367                NOTFND (5200-NOTFND)                               ELXPMCIF
00368                ENDFILE (5200-NOTFND)                              ELXPMCIF
00369                ERROR   (5200-NOTFND)                              ELXPMCIF
00370      END-EXEC.                                                    ELXPMCIF
00371                                                                   ELXPMCIF
00372      MOVE LOW-VALUES TO WS-HEX-00.                                ELXPMCIF
00373                                                                   ELXPMCIF
00374      COMPUTE WS-IO-PARM-ALL-LVL-LEN      =                        ELXPMCIF
00375              GC-GCIOPARM-LEN   +                                  ELXPMCIF
00376              GC-GCTABULR-ABM-FIXED-LEN +                          ELXPMCIF
00377              (GC-GCTABULR-ABM-VARY-LEN *                          ELXPMCIF
00378               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     ELXPMCIF
00379                                                                   ELXPMCIF
00380                                                                   ELXPMCIF
00381      MOVE PMCI-ABM-SLOT TO WS-ABM-SLOT.                           ELXPMCIF
00382                                                                   ELXPMCIF
00383      EXEC CICS READ                                               ELXPMCIF
00384                DATASET(GC-GCTABULR-DDNAME)                        ELXPMCIF
00385                SET    (ABM-POINTER)                               ELXPMCIF
00386                RIDFLD (ABM-KEY)                                   ELXPMCIF
00387      END-EXEC.                                                    ELXPMCIF
00388                                                                   ELXPMCIF
00389      SET ADDRESS OF ABM-RECORD TO ABM-POINTER.                    ELXPMCIF
00390                                                                   ELXPMCIF
00391                                                                   ELXPMCIF
00392      PERFORM 5100-SEARCH-INT-DESC VARYING                         ELXPMCIF
00393                SUB1 FROM 1 BY 1                                   ELXPMCIF
00394                UNTIL SUB1 > GAA-ENTRY-COUNT.                      ELXPMCIF
00395                                                                   ELXPMCIF
00396  5000-EXIT.   EXIT.                                               ELXPMCIF
00397 /***********************************************************      ELXPMCIF
00398 * 5100-SEARCH-INT-DESC                                     *      ELXPMCIF
00399 *    CHECK INTERNAL DESCRIPTORS                            *      ELXPMCIF
00400 * 1. SEARCH FOR SPECIFIC INTERNAL DESCRIPTORS:             *      ELXPMCIF
00401 *    1) MEDDEDMAX - IS FOR INPATIENT HOSPITAL DEDUCTIBLE   *      ELXPMCIF
00402 *    2) ACUTECOIN - IS FOR INPATIENT HOSPITAL COINSURANCE  *      ELXPMCIF
00403 *    3) MEDLIFEDA - IS FOR INPATIENT HOSPITAL LIFETIME     *      ELXPMCIF
00404 *                   RESERVE                                *      ELXPMCIF
00405 *    4) ECFCOINS  - IS FOR SKILLED NURSING FACILITY        *      ELXPMCIF
00406 *                   COINSURANCE                            *      ELXPMCIF
00407 *  IN ORDER TO HAVE ANY OF THESE BENIFITS, AN IBGR TABULAR *      ELXPMCIF
00408 *  MUST EXIST. THE INTERNAL TABULARS ARE ON THE TABULAR    *      ELXPMCIF
00409 *  IN ALPHABETICAL ORDER - IBGR, IDGD, IPGN, ETC.          *      ELXPMCIF
00410 *  BUT, A GIVEN INTERNAL TABULAR MAY NOT EXIST.            *      ELXPMCIF
00411 *                                                          *      ELXPMCIF
00412 ************************************************************      ELXPMCIF
00413 *                                                                 ELXPMCIF
00414  5100-SEARCH-INT-DESC.                                            ELXPMCIF
00415      EVALUATE GAA-BAMA-INTERNAL-DESCRIPTOR(SUB1)                  ELXPMCIF
00416          WHEN 'ACUTECOIN'                                         ELXPMCIF
00417               IF GAA-INT-SLOT(SUB1, 1) > 0 AND                    ELXPMCIF
00418                  GAA-INT-ID(SUB1, 1) EQUAL '#IBGR'                ELXPMCIF
00419                  MOVE 'Y' TO PMCI-PARTA-COIN                      ELXPMCIF
00420               END-IF                                              ELXPMCIF
00421          WHEN 'ECFCOINS'                                          ELXPMCIF
00422               IF GAA-INT-SLOT(SUB1, 1) > 0 AND                    ELXPMCIF
00423                  GAA-INT-ID(SUB1, 1) EQUAL '#IBGR'                ELXPMCIF
00424                  MOVE 'Y' TO PMCI-PARTA-SNF-COIN                  ELXPMCIF
00425               END-IF                                              ELXPMCIF
00426          WHEN 'MEDDEDMAX'                                         ELXPMCIF
00427               IF GAA-INT-SLOT(SUB1, 1) > 0 AND                    ELXPMCIF
00428                  GAA-INT-ID(SUB1, 1) EQUAL '#IBGR'                ELXPMCIF
00429                  MOVE 'Y' TO PMCI-PARTA-DEDUCT                    ELXPMCIF
00430               END-IF                                              ELXPMCIF
00431          WHEN 'MEDLIFEDA'                                         ELXPMCIF
00432               IF GAA-INT-SLOT(SUB1, 1) > 0 AND                    ELXPMCIF
00433                  GAA-INT-ID(SUB1, 1) EQUAL '#IBGR'                ELXPMCIF
00434                  MOVE 'Y' TO PMCI-PARTA-LIFE                      ELXPMCIF
00435               END-IF                                              ELXPMCIF
00436      END-EVALUATE.                                                ELXPMCIF
00437 ************************************************************      ELXPMCIF
00438 * 5200-NOTFND. ALL PURPOSE ERROR ROUTINE.                  *      ELXPMCIF
00439 ************************************************************      ELXPMCIF
00440  5200-NOTFND.                                                     ELXPMCIF
00441         MOVE 'C' TO PMCI-PARTA-DEDUCT                             ELXPMCIF
00442                     PMCI-PARTA-COIN                               ELXPMCIF
00443                     PMCI-PARTA-LIFE                               ELXPMCIF
00444                     PMCI-PARTA-SNF-COIN                           ELXPMCIF
00445                     PMCI-PARTB-BCBS-DED                           ELXPMCIF
00446                     PMCI-PARTB-ANN-DED.                           ELXPMCIF
00447         GO TO 5000-EXIT.                                          ELXPMCIF
00448 ************************************************************      ELXPMCIF
00449 * TEMPORARY CODE FOR ACP                                          ELXPMCIF
00450 ************************************************************      ELXPMCIF
00451  8000-DETERMINE-ACP-VALUES.                                       ELXPMCIF
00452      IF PMCI-ACCUM-CO-PAY-CALL                                    ELXPMCIF
00453        SET EMAC-CALL                                              ELXPMCIF
00454            EMMD-CALL                                              ELXPMCIF
00455            EMLF-CALL                                              ELXPMCIF
00456            OFVS-CALL TO TRUE                                      ELXPMCIF
00457      END-IF.                                                      ELXPMCIF
00458 ************************************************************      ELXPMCIF
00459 * DO FREEMAINS DUE TO SHORT ON STROAGE USING M270 TRANS           ELXPMCIF
00460 ************************************************************      ELXPMCIF
00461  9999-DO-FREEMAINS.                                               ELXPMCIF
00462 *    IF CIA-ELSCONIB-PTR    = NULL                                ELXPMCIF
00463 *      CONTINUE                                                   ELXPMCIF
00464 *    ELSE                                                         ELXPMCIF
00465 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSCONIB-PTR)          ELXPMCIF
00466 *       END-EXEC                                                  ELXPMCIF
00467 *    END-IF.                                                      ELXPMCIF
00468 *    IF CIA-ELSCONPB-PTR    = NULL                                ELXPMCIF
00469 *       CONTINUE                                                  ELXPMCIF
00470 *    ELSE                                                         ELXPMCIF
00471 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSCONPB-PTR)          ELXPMCIF
00472 *       END-EXEC                                                  ELXPMCIF
00473 *    END-IF.                                                      ELXPMCIF
00474 *    IF CIA-ELSGRPSP-PTR    = NULL                                ELXPMCIF
00475 *       CONTINUE                                                  ELXPMCIF
00476 *    ELSE                                                         ELXPMCIF
00477 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSGRPSP-PTR)          ELXPMCIF
00478 *       END-EXEC                                                  ELXPMCIF
00479 *    END-IF.                                                      ELXPMCIF
00480 *    IF CIA-ELSIBGR-PTR    = NULL                                 ELXPMCIF
00481 *       CONTINUE                                                  ELXPMCIF
00482 *    ELSE                                                         ELXPMCIF
00483 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSIBGR-PTR)           ELXPMCIF
00484 *       END-EXEC                                                  ELXPMCIF
00485 *    END-IF.                                                      ELXPMCIF
00486 *    IF CIA-ELSIDGD-PTR    = NULL                                 ELXPMCIF
00487 *       CONTINUE                                                  ELXPMCIF
00488 *    ELSE                                                         ELXPMCIF
00489 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSIDGD-PTR)           ELXPMCIF
00490 *       END-EXEC                                                  ELXPMCIF
00491 *    END-IF.                                                      ELXPMCIF
00492 *    IF CIA-ELSIOPM-PTR    = NULL                                 ELXPMCIF
00493 *       CONTINUE                                                  ELXPMCIF
00494 *    ELSE                                                         ELXPMCIF
00495 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSIOPM-PTR)           ELXPMCIF
00496 *       END-EXEC                                                  ELXPMCIF
00497 *    END-IF.                                                      ELXPMCIF
00498 *    IF CIA-ELSIPGN-PTR    = NULL                                 ELXPMCIF
00499 *       CONTINUE                                                  ELXPMCIF
00500 *    ELSE                                                         ELXPMCIF
00501 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSIPGN-PTR)           ELXPMCIF
00502 *       END-EXEC                                                  ELXPMCIF
00503 *    END-IF.                                                      ELXPMCIF
00504 *    IF CIA-ELSIPGP-PTR    = NULL                                 ELXPMCIF
00505 *       CONTINUE                                                  ELXPMCIF
00506 *    ELSE                                                         ELXPMCIF
00507 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSIPGP-PTR)           ELXPMCIF
00508 *       END-EXEC                                                  ELXPMCIF
00509 *    END-IF.                                                      ELXPMCIF
00510 *    IF CIA-ELSIPGT-PTR    = NULL                                 ELXPMCIF
00511 *       CONTINUE                                                  ELXPMCIF
00512 *    ELSE                                                         ELXPMCIF
00513 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSIPGT-PTR)           ELXPMCIF
00514 *       END-EXEC                                                  ELXPMCIF
00515 *    END-IF.                                                      ELXPMCIF
00516 *    IF CIA-ELSIPGS-PTR    = NULL                                 ELXPMCIF
00517 *       CONTINUE                                                  ELXPMCIF
00518 *    ELSE                                                         ELXPMCIF
00519 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSIPGS-PTR)           ELXPMCIF
00520 *       END-EXEC                                                  ELXPMCIF
00521 *    END-IF.                                                      ELXPMCIF
00522 *    IF CIA-ELSKEYS-PTR    = NULL                                 ELXPMCIF
00523 *       CONTINUE                                                  ELXPMCIF
00524 *    ELSE                                                         ELXPMCIF
00525 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSKEYS-PTR)           ELXPMCIF
00526 *       END-EXEC                                                  ELXPMCIF
00527 *    END-IF.                                                      ELXPMCIF
00528 *    IF CIA-ELSPMCID-PTR    = NULL                                ELXPMCIF
00529 *       CONTINUE                                                  ELXPMCIF
00530 *    ELSE                                                         ELXPMCIF
00531 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-ELSPMCID-PTR)          ELXPMCIF
00532 *       END-EXEC                                                  ELXPMCIF
00533 *    END-IF.                                                      ELXPMCIF
00534 *    IF CIA-GCBENPRV-PTR    = NULL                                ELXPMCIF
00535 *       CONTINUE                                                  ELXPMCIF
00536 *    ELSE                                                         ELXPMCIF
00537 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-GCBENPRV-PTR)          ELXPMCIF
00538 *       END-EXEC                                                  ELXPMCIF
00539 *    END-IF.                                                      ELXPMCIF
00540 *    IF CIA-GCCONTR-PTR    = NULL                                 ELXPMCIF
00541 *      CONTINUE                                                   ELXPMCIF
00542 *    ELSE                                                         ELXPMCIF
00543 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-GCCONTR-PTR)           ELXPMCIF
00544 *       END-EXEC                                                  ELXPMCIF
00545 *    END-IF.                                                      ELXPMCIF
00546 *    IF CIA-GCDATES-PTR    = NULL                                 ELXPMCIF
00547 *       CONTINUE                                                  ELXPMCIF
00548 *    ELSE                                                         ELXPMCIF
00549 *       EXEC CICS FREEMAIN DATAPOINT(CIA-GCDATES-PTR)             ELXPMCIF
00550 *       END-EXEC                                                  ELXPMCIF
00551 *    END-IF.                                                      ELXPMCIF
00552 *    IF CIA-GCGRPSPC-PTR    = NULL                                ELXPMCIF
00553 *       CONTINUE                                                  ELXPMCIF
00554 *    ELSE                                                         ELXPMCIF
00555 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-GCGRPSPC-PTR)          ELXPMCIF
00556 *       END-EXEC                                                  ELXPMCIF
00557 *    END-IF.                                                      ELXPMCIF
00558 *    IF CIA-GCTABULR-PTR    = NULL                                ELXPMCIF
00559 *       CONTINUE                                                  ELXPMCIF
00560 *    ELSE                                                         ELXPMCIF
00561 *       EXEC CICS FREEMAIN DATAPOINTER(CIA-GCTABULR-PTR)          ELXPMCIF
00562 *       END-EXEC                                                  ELXPMCIF
00563 *    END-IF.                                                      ELXPMCIF
00564      IF EIBTRNID NOT = 'ELRM'                                     ELXPMCIF
00565        IF PMCI-ACCUM-PTR = NULL                                   ELXPMCIF
00566           CONTINUE                                                ELXPMCIF
00567        ELSE                                                       ELXPMCIF
00568           EXEC CICS FREEMAIN DATAPOINTER(PMCI-ACCUM-PTR)          ELXPMCIF
00569           END-EXEC                                                ELXPMCIF
00570      END-IF.                                                      ELXPMCIF
