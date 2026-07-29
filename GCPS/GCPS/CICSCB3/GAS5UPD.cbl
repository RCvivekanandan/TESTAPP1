00001  IDENTIFICATION DIVISION.                                         02/18/05
00002  PROGRAM-ID. GAS5UPD.                                             GAS5UPD 
00003 **** THIS IS A COBOL/2 PROGRAM ***                                   LV005
00004  AUTHOR. GARY D MULLINGS.                                         GAS5UPD 
00005  DATE-WRITTEN.   SEPT 1998.                                       GAS5UPD 
00006  DATE-COMPILED.                                                   GAS5UPD 
00007      SKIP3                                                        GAS5UPD 
00008 ******************************************************************GAS5UPD 
00009 *   GAS5UPD         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *GAS5UPD 
00010 *                         ACCUMULATOR COPAY  - #ACP              *GAS5UPD 
00011 *                                                                *GAS5UPD 
00012 *     THIS PROGRAM WILL PERFORM ADD/CHANGE/DELETE MAINTENANCE TO *GAS5UPD 
00013 *   ENTRIES ON THE ALL LEVEL TABULAR RECORD.  THE TABULAR RECORD *GAS5UPD 
00014 *   CAN CONTAIN UP TO 29 ENTRIES IN A TABLE, EACH ENTRY HAS A    *GAS5UPD 
00015 *   NUMBER OF FIELDS AND ANOTHER SMALL TABLE, THIS 2NDARY TABLE  *GAS5UPD 
00016 *   IS A POINTER TO AN INTERNAL TABULAR RECORD.  THE PROGRAM     *GAS5UPD 
00017 *   OPERATES IN TWO MODES AN ADD/CHANGE AND A CHANGE/DELETE MODE.*GAS5UPD 
00018 *                                                                *GAS5UPD 
00019 *     THE CHG/DEL SCREEN WILL DISPLAY AN ENTRY CURRENTLY ON THE  *GAS5UPD 
00020 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN CHANGE ANY *GAS5UPD 
00021 *   FIELD OR ADD, CHANGE, OR DELETE AN INTERNAL TABULAR; THERE IS*GAS5UPD 
00022 *   ALSO THE OPTION OF DELETING THE WHOLE ENTRY IN THE TABULAR,  *GAS5UPD 
00023 *   INTERNAL TABULARS INCLUDED, THIS OPTION CAN BE SELECTED BY   *GAS5UPD 
00024 *   PLACING A 'D' IN THE DELETE OPTION FIELD.                    *GAS5UPD 
00025 *                                                                *GAS5UPD 
00026 *    THE CHG/ADD SCREEN WILL BE SHOWN THE OPERATOR WHEN THEY WANT*GAS5UPD 
00027 *   TO ADD A NEW ENTRY INTO THE TABLE. FROM HERE THE OPERATOR CAN*GAS5UPD 
00028 *   FILL THE ENTRY, THEN REVIEW AND CHANGE THE NEW ENTRY.  AFTER *GAS5UPD 
00029 *   THE OPERATOR KEYS ENTER ON THE REVIEW SCREEN, THE PROGRAM    *GAS5UPD 
00030 *   ASSUMES THAT THEY WANT TO ADD ANOTHER ENTRY AND SO DISPLAYS  *GAS5UPD 
00031 *   THE SKELETON FOR THE OPERATOR TO OVERLAY.                    *GAS5UPD 
00032 *                                                                *GAS5UPD 
00033 *   FUNC CODE: GAS5                                              *GAS5UPD 
00034 *                          ********************************      *GAS5UPD 
00035 *                          *   THIS MAPSET IS SHARED BY   *      *GAS5UPD 
00036 *                          *   THE FOLLOWING MODULES:     *      *GAS5UPD 
00037 *                          *   1. GA1BPGM                 *      *GAS5UPD 
00038 *                          *   2. GA1CPGM                 *      *GAS5UPD 
00039 *                          *   3. GA1DPGM                 *      *GAS5UPD 
00040 *                          *   4. GA1EPGM                 *      *GAS5UPD 
00041 *                          *   5. GA1PPGM                 *      *GAS5UPD 
00042 *                          *   6. GASEDIT1                *      *GAS5UPD 
00043 *   MAPSET:    GA1XSETC ==>*   7. GACDEPGM                *      *GAS5UPD 
00044 *                          *   8. GK1BPGM                 *      *GAS5UPD 
00045 *                          *   9. GK1CPGM                 *      *GAS5UPD 
00046 *                          *  10. GK1DPGM                 *      *GAS5UPD 
00047 *                          *  11. GK1EPGM                 *      *GAS5UPD 
00048 *                          *  12. GK1PPGM                 *      *GAS5UPD 
00049 *                          *  13. GAS1UPD                 *      *GAS5UPD 
00050 *                          *  14. GAS2UPD                 *      *GAS5UPD 
00051 *                          *  15. GAS5UPD                 *      *GAS5UPD 
00052 *                          *  16. GAS4UPD                 *      *GAS5UPD 
00053 *                          ********************************      *GAS5UPD 
00054 *                                                                *GAS5UPD 
00055 *   FILES:     GCPSWORK           GCGRPSPC                       *GAS5UPD 
00056 *              GCTABULR           GCSTABLR                       *GAS5UPD 
00057 *              GCCONTR            GCSPROVN                       *GAS5UPD 
00058 *                                                                *GAS5UPD 
00059 ******************************************************************GAS5UPD 
00060                                                                   GAS5UPD 
00061 /    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS5UPD 
00062 *    *-*         U P D A T E   H I S T O R Y         *-*          GAS5UPD 
00063 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS5UPD 
00064                                                                   GAS5UPD 
00065 *  NUM-* *-DATE-* *WHO* *-----------DESCRIPTION------------------*GAS5UPD 
00066 *                                                                *GAS5UPD 
00067 *  D341  9/28/98  GDM  INITIAL MODULE                           * GAS5UPD 
00068 *  XXXX  3/22/99  KJD  FIX IF STATEMENTS WITH MISSING PERIODS    *GAS5UPD 
00069 *                                                                *GAS5UPD 
00070 *  XXXX  7/07/00  GSP  ADD LOGIC FOR NEW #IPGS INTERNAL TABULAR. *GAS5UPD 
00071 *                                                                *GAS5UPD 
00072 *  D352  09/21/00 GDM  ADD NACCUM IDENTIFIER                     *GAS5UPD 
00073 *                                                                *GAS5UPD 
00074 *        11/29/00 GSP  ADD LOGIC TO DISPLAY MESSAGE IF A 6TH     *GAS5UPD 
00075 *                      INTERNAL TABULAR IS ATTEMPTED TO BE       *GAS5UPD 
00076 *                      ADDED.                                    *GAS5UPD 
00077 *                                                                *GAS5UPD 
00078 *        01/12/01 GSP ADD LOGIC TO PREVENT INTERNAL TABULAR      *GAS5UPD 
00079 *                     COUNT FROM BEING INCREASED TO GREATER      *GAS5UPD 
00080 *                     THAN 5.                                    *GAS5UPD 
00081 *                                                                *GAS5UPD 
00082 *        11/16/01 AKK ADD SUPPORT FOR 4 NEW BITS, TWO FOR        *GAS5UPD 
00083 *                     EMER AND TWO FOR SERIOUS MENTAL ILLNESS    *GAS5UPD 
00084 *                                                                *GAS5UPD 
00085 *D365B 06/03/02   JP  ADD COMBINATION APPLIED INDICATOR (CAPI)   *GAS5UPD 
00086 *                                                                *GAS5UPD 
00087 *  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *GAS5UPD 
00088 *                         DETERMINATION (SABD)                   *GAS5UPD 
00089 *                                                                *GAS5UPD 
00090 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GAS5UPD 
00091 *                                                                *GAS5UPD 
00092 *            06-13-03   DAF   CORRECTED PROGRAM SO THAT THE      *GAS5UPD 
00093 *                             TABULAR ID IS ON THE 'C6' RECORD   *GAS5UPD 
00094 *                             INSTEAD OF THE BENEFIT PROVISION ID*GAS5UPD 
00095 *                             AND THE TABULAR SLOT NUMBER IS ON  *GAS5UPD 
00096 *                             THE 'C6' RECORD INSTEAD OF THE     *GAS5UPD 
00097 *                             BENEFIT PROVISION SLOT NUMBER      *GAS5UPD 
00098 *                             USE COPYBOOK GCTIPGPC INSTEAD OF   *GAS5UPD 
00099 *                             GCTIPGTC                           *GAS5UPD 
00100 *                                                                *GAS5UPD 
00090 * P09400     11-07-06   GF    ADD ASCEND/DESCEND AND BISCENDING  *GAS5UPD 
00091 *                             INDICATORS                         *GAS5UPD 
00100 *                                                                *GAS5UPD 
00090 *            10-15-10   MJL   ALLOW 'UNL' VALUE.                 *GAS5UPD 
00100 *                                                                *GAS5UPD 
TB0424* PRD0000172 04-18-24   TEB   CORRECTED BUG THAT WAS USING WRONG *GAS5UPD 
TB0424* BBDA-58089                  FIELDNAME IN AN IF STATEMENT.      *GAS5UPD 
TB0424* INC4008009                  (LABELED AS TB0424)                *GAS5UPD 
00101 ******************************************************************GAS5UPD 
00102                                                                   GAS5UPD 
00103 **************************************************************    GAS5UPD 
00104 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS5UPD 
00105 * - DEAD CODE ELIMINATION.                                        GAS5UPD 
      *                                                                         
      * P21595  09/19/16  HSB  CHANGES FOR NEW FIELDS BEN TYPE CODE,  *         
      *                        BENEFIT TIER CODE,BENEFIT TIER LEVEL.  *         
SI0724*                                                               *         
SI0724* P56703  05/08/24  SI   RECOMPILE - PEAQ COPYBOOK EXPANSION    *         
SI0724*                        COPY ABM, ACP, ACL, ADL, AOL,          *         
SI0724*                        GCCDRLEN                               *         
00106 ***************************************************************** GAS5UPD 
00107                                                                   GAS5UPD 
00108      SKIP3                                                        GAS5UPD 
00109  ENVIRONMENT DIVISION.                                            GAS5UPD 
00110 /    D A T A   D I V I S I O N                                    GAS5UPD 
00111  DATA DIVISION.                                                   GAS5UPD 
00112  WORKING-STORAGE SECTION.                                         GAS5UPD 
00113  01  WS-BEGIN                    PIC X(24)  VALUE                 GAS5UPD 
00114      '***GAS5UPD WS BEGINS***'.                                   GAS5UPD 
00115                                                                   GAS5UPD 
00116 *    T I T L E   L I N E S                                        GAS5UPD 
00117  01  WS-TITLE-LINES.                                              GAS5UPD 
00118  COPY GCMHLINE.                                                   GAS5UPD 
00119 /    A L T E R N A T I V E   W O R K F I L E   K E Y S            GAS5UPD 
00120  01  FILLER                      PIC X(32)  VALUE                 GAS5UPD 
00121      '*** ALTERNATIVE WORKFILE KEY ***'.                          GAS5UPD 
00122  01  SAVE-WS-ALT-WORKFILE-KEYS.                                   GAS5UPD 
00123      05 FILLER                   PIC X(63) VALUE SPACES.          GAS5UPD 
00124                                                                   GAS5UPD 
00125  01  SAVE-RESTORE-KEY.                                            GAS5UPD 
00126      05 WS-SV-RESTO-KY           PIC X(63) VALUE SPACES.          GAS5UPD 
00127                                                                   GAS5UPD 
00128  01  WS-ALT-WORKFILE-KEYS.                                        GAS5UPD 
00129  COPY GCWRKKEY.                                                   GAS5UPD 
00130                                                                   GAS5UPD 
00131                                                                   GAS5UPD 
00132 *   D A T E   F O R M A T T I N G   C O M M A R E A               GAS5UPD 
00133                                                                   GAS5UPD 
00134  01  HGADATES-COMMAREA.                                           GAS5UPD 
00135  COPY HGCDAT01.                                                   GAS5UPD 
00136                                                                   GAS5UPD 
00137 *    W O R K F I E L D S ,   A N D   S W I T C H E S              GAS5UPD 
00138  01  WS-WORK-FIELDS.                                              GAS5UPD 
00139                                                                   GAS5UPD 
00140      05  WS-SPACES-ZEROS.                                         GAS5UPD 
00141        10  WS-SPACES-ZEROS-SPACES       PIC X(6)  VALUE SPACES.   GAS5UPD 
00142        10  WS-SPACES-ZEROS-ZEROS        PIC S9(7) COMP-3          GAS5UPD 
00143                                                   VALUE ZEROS.    GAS5UPD 
00144                                                                   GAS5UPD 
00145      05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    GAS5UPD 
00146                                                                   GAS5UPD 
00147      05  WS-TAB-PROV-COPY-SLOT          PIC S9(7) COMP-3.         GAS5UPD 
00148      05  WS-INTL-TAB-ID.                                          GAS5UPD 
00149        10  WS-INTL-TAB-TAB-ID           PIC X(6).                 GAS5UPD 
00150        10  WS-INTL-TAB-TAB-SLOT         PIC S9(7) COMP-3.         GAS5UPD 
00151      05  WS-SAVE-INTL-TAB.                                        GAS5UPD 
00152        10  WS-SAVE-INTL-TAB-ID          PIC X(6).                 GAS5UPD 
00153        10  WS-SAVE-INTL-TAB-SLOT        PIC S9(7) COMP-3.         GAS5UPD 
00154                                                                   GAS5UPD 
00155      05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  GAS5UPD 
00156      05  WS-QUOTIENT                   PIC 999  COMP-3.           GAS5UPD 
00157      05  WS-REMAINDER                  PIC 999  COMP-3.           GAS5UPD 
00158                                                                   GAS5UPD 
00159      05  WS-CDE-REQUEST-CODES.                                    GAS5UPD 
00160          10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.GAS5UPD 
00161          10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.GAS5UPD 
00162          10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.GAS5UPD 
00163          10  WS-REQUEST-4900-CNTL-UPDATE    PIC X(4) VALUE '4900'.GAS5UPD 
00164                                                                   GAS5UPD 
00165      05  WS-INTRNL-TABS-TO-CHG-CNT          PIC S9   COMP-3.      GAS5UPD 
00166      05  WS-INT-TAB-CHANGE-INDICATOR        PIC XX   VALUE SPACE. GAS5UPD 
00167        88  WS-INT-DESCRP-CHG-TO-NON-PROD        VALUE 'PN'.       GAS5UPD 
00168        88  WS-INT-DESCRP-CHG-BACK-TO-PROD       VALUE 'NP'.       GAS5UPD 
00169        88  WS-INT-DESCRP-NOCHG-AT-PROD          VALUE '  '.       GAS5UPD 
00170        88  WS-INT-DESCRP-NOCHG-AT-NONPROD       VALUE 'NN'.       GAS5UPD 
00171                                                                   GAS5UPD 
00172 *    I N T E R N A L   T A B U L A R   P R O G R A M   N A M E    GAS5UPD 
00173  01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  GAS5UPD 
00174                                                                   GAS5UPD 
00175                                                                   GAS5UPD 
00176 ** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          GAS5UPD 
00177  01  WS-ENTRY                          PIC X(176).                GAS5UPD 
00178      SKIP3                                                        GAS5UPD 
00179 /   A T T R I B U T E S                                           GAS5UPD 
00180  COPY DFHBMSCA.                                                   GAS5UPD 
00181      02  DFHBMABF                PIC X VALUE '9'.                 GAS5UPD 
00182 /   A T T E N T I O N   I D E N T I F I E R S                     GAS5UPD 
00183  COPY DFHAID.                                                     GAS5UPD 
00184 /   R E C O R D   L E N G T H S                                   GAS5UPD 
00185                                                                   GAS5UPD 
00186  01  WS-RECORD-LENGTHS.                                           GAS5UPD 
00187     05 WS-COMMON-WORKAREA-LEN         PIC S9(4) COMP  VALUE +550. GAS5UPD 
00188     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. GAS5UPD 
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +7744.GAS5UPD 
SI0724    05 WS-COPY-TABLE-LEN              PIC S9(4) COMP VALUE +30800.GAS5UPD 
00190     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00191     05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00192     05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00193     05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00194     05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00195     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00196     05 WS-WF-INTR-TAB-LEN             PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00197     05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00198     05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00199     05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   GAS5UPD 
00200     05 WS-NEW-OCCR-ON-WF              PIC X   VALUE SPACE.        GAS5UPD 
00201                                                                   GAS5UPD 
00202 ******************************************************************GAS5UPD 
00203 ** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **GAS5UPD 
00204 ******************************************************************GAS5UPD 
00205  01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  GAS5UPD 
00206  01  CURNT-OCURS-PKD             PIC 9(4).                        GAS5UPD 
00207  01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            GAS5UPD 
00208      05  FILLER                  PIC XX.                          GAS5UPD 
00209      05  CURNT-OCCURS-OUT        PIC XX.                          GAS5UPD 
00210                                                                   GAS5UPD 
00211  01  TOTAL-OCURS-UNK             PIC 9(5).                        GAS5UPD 
00212  01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            GAS5UPD 
00213      05  FILLER                  PIC XXX.                         GAS5UPD 
00214      05  TOTAL-OCCURS-OUT        PIC XX.                          GAS5UPD 
00215 /    G . C .   R E C O R D S   L E N G T H S                      GAS5UPD 
00216  01  WS-GC-RECORD-LENGTHS.                                        GAS5UPD 
00217      COPY GCCDRLEN.                                               GAS5UPD 
00218                                                                   GAS5UPD 
00219 /    A B E N D   A R E A                                          GAS5UPD 
00220  01  WS-01-ABEND-AREA.                                            GAS5UPD 
00221      05  FILLER                   PIC X(16)  VALUE                GAS5UPD 
00222          '** ABEND AREA **'.                                      GAS5UPD 
00223                                                                   GAS5UPD 
00224      05  WS-ABCODE-CODES-AND-MSG.                                 GAS5UPD 
00225          10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. GAS5UPD 
00226          10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. GAS5UPD 
00227          10  WS-ABCODE-1PF1             PIC X(04)  VALUE  '1PF1'. GAS5UPD 
00228          10  WS-ABCODE-1PF1-MSG         PIC X(79)  VALUE          GAS5UPD 
00229              '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABUGAS5UPD 
00230 -            'LAR.  CONTACT SYSTEMS ***  '.                       GAS5UPD 
00231          10  WS-ABCODE-1PF2             PIC X(04)  VALUE  '1PF2'. GAS5UPD 
00232          10  WS-ABCODE-1PF2-MSG         PIC X(79)  VALUE          GAS5UPD 
00233              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS5UPD 
00234 -            ' CONTACT SYSTEMS ***       '.                       GAS5UPD 
00235          10  WS-ABCODE-1PF3             PIC X(04)  VALUE  '1PF3'. GAS5UPD 
00236          10  WS-ABCODE-1PF3-MSG         PIC X(79)  VALUE          GAS5UPD 
00237              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS5UPD 
00238 -            ' CONTACT SYSTEMS ***       '.                       GAS5UPD 
00239          10  WS-ABCODE-1PF4             PIC X(04)  VALUE  '1PF4'. GAS5UPD 
00240          10  WS-ABCODE-1PF4-MSG         PIC X(79)  VALUE          GAS5UPD 
00241              '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTAGAS5UPD 
00242 -            'CT SYSTEMS ***             '.                       GAS5UPD 
00243          10  WS-ABCODE-1PF5             PIC X(04)  VALUE  '1PF5'. GAS5UPD 
00244          10  WS-ABCODE-1PF5-MSG         PIC X(79)  VALUE          GAS5UPD 
00245              'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFIGAS5UPD 
00246 -            'LE.  PLEASE CONTACT SYSTEMS'.                       GAS5UPD 
00247          10  WS-ABCODE-1PF6             PIC X(04)  VALUE  '1PF6'. GAS5UPD 
00248          10  WS-ABCODE-1PF6-MSG         PIC X(79)  VALUE          GAS5UPD 
00249              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFILGAS5UPD 
00250 -            'E.  PLEASE CONTACT SYSTEMS '.                       GAS5UPD 
00251          10  WS-ABCODE-1PF7             PIC X(04)  VALUE  '1PF7'. GAS5UPD 
00252          10  WS-ABCODE-1PF7-MSG         PIC X(79)  VALUE          GAS5UPD 
00253              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS5UPD 
00254 -            ' SYSTEMS ***               '.                       GAS5UPD 
00255          10  WS-ABCODE-1PF9             PIC X(04)  VALUE  '1PF9'. GAS5UPD 
00256          10  WS-ABCODE-1PF9-MSG         PIC X(79)  VALUE          GAS5UPD 
00257              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILEGAS5UPD 
00258 -            '.  PLEASE CONTACT SYSTEMS  '.                       GAS5UPD 
00259          10  WS-ABCODE-1PFA             PIC X(04)  VALUE  '1PFA'. GAS5UPD 
00260          10  WS-ABCODE-1PFA-MSG         PIC X(79)  VALUE          GAS5UPD 
00261              '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE GAS5UPD 
00262 -            'CONTACT SYSTEMS ***        '.                       GAS5UPD 
00263          10  WS-ABCODE-1PFB             PIC X(04)  VALUE  '1PFB'. GAS5UPD 
00264          10  WS-ABCODE-1PFB-MSG         PIC X(79)  VALUE          GAS5UPD 
00265              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS5UPD 
00266 -            ' SYSTEMS ***               '.                       GAS5UPD 
00267          10  WS-ABCODE-1PFC             PIC X(04)  VALUE  '1PFC'. GAS5UPD 
00268          10  WS-ABCODE-1PFC-MSG         PIC X(79)  VALUE          GAS5UPD 
00269              '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTACGAS5UPD 
00270 -            'T SYSTEMS ***              '.                       GAS5UPD 
00271          10  WS-ABCODE-1PFJ             PIC X(04)  VALUE  '1PFJ'. GAS5UPD 
00272          10  WS-ABCODE-1PFJ-MSG         PIC X(79)  VALUE          GAS5UPD 
00273              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS5UPD 
00274 -            ' SYSTEMS ***               '.                       GAS5UPD 
00275          10  WS-ABCODE-1PFK             PIC X(04)  VALUE  '1PFK'. GAS5UPD 
00276          10  WS-ABCODE-1PFK-MSG         PIC X(79)  VALUE          GAS5UPD 
00277              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS5UPD 
00278 -            ' SYSTEMS ***               '.                       GAS5UPD 
00279          10  WS-ABCODE-1PFL             PIC X(04)  VALUE  '1PFL'. GAS5UPD 
00280          10  WS-ABCODE-1PFL-MSG         PIC X(79)  VALUE          GAS5UPD 
00281              '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TOGAS5UPD 
00282 -            'MENU.  CONTACT SYSTEMS *** '.                       GAS5UPD 
00283          10  WS-ABCODE-1PFM             PIC X(04)  VALUE  '1PFM'. GAS5UPD 
00284          10  WS-ABCODE-1PFM-MSG         PIC X(79)  VALUE          GAS5UPD 
00285              '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  GAS5UPD 
00286 -            'MENU.  CONTACT SYSTEMS *** '.                       GAS5UPD 
00287          10  WS-ABCODE-1PFN             PIC X(04)  VALUE  '1PFN'. GAS5UPD 
00288          10  WS-ABCODE-1PFN-MSG         PIC X(79)  VALUE          GAS5UPD 
00289              '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO MGAS5UPD 
00290 -            'ENU.  CONTACT SYSTEMS ***  '.                       GAS5UPD 
00291          10  WS-ABCODE-1PFO             PIC X(04)  VALUE  '1PFO'. GAS5UPD 
00292          10  WS-ABCODE-1PFO-MSG         PIC X(79)  VALUE          GAS5UPD 
00293              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS5UPD 
00294 -            ' SYSTEMS ***               '.                       GAS5UPD 
00295          10  WS-ABCODE-1PFP             PIC X(04)  VALUE  '1PFP'. GAS5UPD 
00296          10  WS-ABCODE-1PFP-MSG         PIC X(79)  VALUE          GAS5UPD 
00297              '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACTGAS5UPD 
00298 -            ' SYSTEMS ***               '.                       GAS5UPD 
00299          10  WS-ABCODE-1PFQ             PIC X(04)  VALUE  '1PFQ'. GAS5UPD 
00300          10  WS-ABCODE-1PFQ-MSG         PIC X(79)  VALUE          GAS5UPD 
00301              '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTAGAS5UPD 
00302 -            'CT SYSTEMS ***             '.                       GAS5UPD 
00303          10  WS-ABCODE-1PFR             PIC X(04)  VALUE  '1PFR'. GAS5UPD 
00304          10  WS-ABCODE-1PFR-MSG         PIC X(79)  VALUE          GAS5UPD 
00305              '*** ERROR READING W/F ALL LVL TAB.    PLEASE CONTACTGAS5UPD 
00306 -            ' SYSTEMS ***               '.                       GAS5UPD 
00307          10  WS-ABCODE-1PFS             PIC X(04)  VALUE  '1PFS'. GAS5UPD 
00308          10  WS-ABCODE-1PFS-MSG         PIC X(79)  VALUE          GAS5UPD 
00309              '*** ERROR REWRITING W/F ALL LVL TAB.  PLEASE CONTACTGAS5UPD 
00310 -            ' SYSTEMS ***               '.                       GAS5UPD 
00311          10  WS-ABCODE-1PFT             PIC X(04)  VALUE  '1PFT'. GAS5UPD 
00312          10  WS-ABCODE-1PFT-MSG         PIC X(79)  VALUE          GAS5UPD 
00313              '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACTGAS5UPD 
00314 -            ' SYSTEMS ***               '.                       GAS5UPD 
00315          10  WS-ABCODE-1PFU             PIC X(04)  VALUE  '1PFU'. GAS5UPD 
00316          10  WS-ABCODE-1PFU-MSG         PIC X(79)  VALUE          GAS5UPD 
00317              '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTAGAS5UPD 
00318 -            'CT SYSTEMS ***             '.                       GAS5UPD 
00319          10  WS-ABCODE-1PL1             PIC X(04)  VALUE  '1PL1'. GAS5UPD 
00320          10  WS-ABCODE-1PL1-MSG         PIC X(79)  VALUE          GAS5UPD 
00321              '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***GAS5UPD 
00322 -            '                           '.                       GAS5UPD 
00323          10  WS-ABCODE-1PL2             PIC X(04)  VALUE  '1PL2'. GAS5UPD 
00324          10  WS-ABCODE-1PL2-MSG         PIC X(79)  VALUE          GAS5UPD 
00325              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS5UPD 
00326 -            'MS ***                     '.                       GAS5UPD 
00327          10  WS-ABCODE-1PL3             PIC X(04)  VALUE  '1PL3'. GAS5UPD 
00328          10  WS-ABCODE-1PL3-MSG         PIC X(79)  VALUE          GAS5UPD 
00329              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS5UPD 
00330 -            'EMS ***                    '.                       GAS5UPD 
00331          10  WS-ABCODE-1PLX             PIC X(04)  VALUE  '1PLX'. GAS5UPD 
00332          10  WS-ABCODE-1PLX-MSG         PIC X(79)  VALUE          GAS5UPD 
00333              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS5UPD 
00334 -            'MS ***                     '.                       GAS5UPD 
00335          10  WS-ABCODE-1PP1             PIC X(04)  VALUE  '1PP1'. GAS5UPD 
00336          10  WS-ABCODE-1PP1-MSG         PIC X(79)  VALUE          GAS5UPD 
00337              '????????????????????????????????????????????????????GAS5UPD 
00338 -            '???????????????????????????'.                       GAS5UPD 
00339                                                                   GAS5UPD 
00340 /*****************************************************************GAS5UPD 
00341 *    WT-01   M E S S A G E   T A B L E                            GAS5UPD 
00342 ******************************************************************GAS5UPD 
00343  01  WT-01-TABLE.                                                 GAS5UPD 
00344      05  FILLER                  PIC X(16) VALUE                  GAS5UPD 
00345          '* WT-01-TABLE  *'.                                      GAS5UPD 
00346  01  FILLER.                                                      GAS5UPD 
00347      05  WT-01-MESSAGE-VALUES.                                    GAS5UPD 
00348                                                                   GAS5UPD 
00349 *----------------------------------------------------------------*GAS5UPD 
00350          10  WT-01-ENTRY-001.                                     GAS5UPD 
00351              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00352              15  WT-01-MESSAGE-TEXT-001.                          GAS5UPD 
00353                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00354                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00355                  20  FILLER          PIC X(3)  VALUE  '001'.      GAS5UPD 
00356                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00357                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00358                      '#IBGR HAS BEEN SUCCESSFULLY MAPPED          GAS5UPD 
00359 -                    '                         '.                 GAS5UPD 
00360              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00361 *----------------------------------------------------------------*GAS5UPD 
00362          10  WT-01-ENTRY-002.                                     GAS5UPD 
00363              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00364              15  WT-01-MESSAGE-TEXT-002.                          GAS5UPD 
00365                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00366                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00367                  20  FILLER          PIC X(3)  VALUE  '002'.      GAS5UPD 
00368                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00369                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00370                      '#IPGN HAS BEEN SUCCESSFULLY MAPPED          GAS5UPD 
00371 -                    '                         '.                 GAS5UPD 
00372              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00373 *----------------------------------------------------------------*GAS5UPD 
00374          10  WT-01-ENTRY-003.                                     GAS5UPD 
00375              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00376              15  WT-01-MESSAGE-TEXT-003.                          GAS5UPD 
00377                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00378                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00379                  20  FILLER          PIC X(3)  VALUE  '003'.      GAS5UPD 
00380                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00381                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00382                      '#IPGT HAS BEEN SUCCESSFULLY MAPPED          GAS5UPD 
00383 -                    '                         '.                 GAS5UPD 
00384              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00385 *----------------------------------------------------------------*GAS5UPD 
00386          10  WT-01-ENTRY-004.                                     GAS5UPD 
00387              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00388              15  WT-01-MESSAGE-TEXT-004.                          GAS5UPD 
00389                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00390                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00391                  20  FILLER          PIC X(3)  VALUE  '004'.      GAS5UPD 
00392                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00393                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00394                      '******************* F U T U R E   U S E ****GAS5UPD 
00395 -                    '*************************'.                 GAS5UPD 
00396              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00397 *----------------------------------------------------------------*GAS5UPD 
00398          10  WT-01-ENTRY-005.                                     GAS5UPD 
00399              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00400              15  WT-01-MESSAGE-TEXT-005.                          GAS5UPD 
00401                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00402                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00403                  20  FILLER          PIC X(3)  VALUE  '005'.      GAS5UPD 
00404                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00405                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00406                      '******************* F U T U R E   U S E ****GAS5UPD 
00407 -                    '*************************'.                 GAS5UPD 
00408              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00409 *----------------------------------------------------------------*GAS5UPD 
00410          10  WT-01-ENTRY-006.                                     GAS5UPD 
00411              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00412              15  WT-01-MESSAGE-TEXT-006.                          GAS5UPD 
00413                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00414                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00415                  20  FILLER          PIC X(3)  VALUE  '006'.      GAS5UPD 
00416                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00417                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00418                      'DELETE OPTION MUST BE \
00419 -                    'VALID                    '.                 GAS5UPD 
00420              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00421 *----------------------------------------------------------------*GAS5UPD 
00422          10  WT-01-ENTRY-007.                                     GAS5UPD 
00423              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00424              15  WT-01-MESSAGE-TEXT-007.                          GAS5UPD 
00425                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00426                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00427                  20  FILLER          PIC X(3)  VALUE  '007'.      GAS5UPD 
00428                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00429                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00430                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSGAS5UPD 
00431 -                    ' PF4/PF16 TO CONTINUE    '.                 GAS5UPD 
00432              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00433 *----------------------------------------------------------------*GAS5UPD 
00434          10  WT-01-ENTRY-008.                                     GAS5UPD 
00435              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00436              15  WT-01-MESSAGE-TEXT-008.                          GAS5UPD 
00437                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00438                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00439                  20  FILLER          PIC X(3)  VALUE  '008'.      GAS5UPD 
00440                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00441                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00442                      'GROUP IN CONVERSION STATUS, CANNOT CHANGE HIGAS5UPD 
00443 -                    'GH-LIGHTED ELEMENTS      '.                 GAS5UPD 
00444              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00445 *----------------------------------------------------------------*GAS5UPD 
00446          10  WT-01-ENTRY-009.                                     GAS5UPD 
00447              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00448              15  WT-01-MESSAGE-TEXT-009.                          GAS5UPD 
00449                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00450                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00451                  20  FILLER          PIC X(3)  VALUE  '009'.      GAS5UPD 
00452                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00453                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00454                      'INVALID PFKEY SELECTION                     GAS5UPD 
00455 -                    '                         '.                 GAS5UPD 
00456              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00457 *----------------------------------------------------------------*GAS5UPD 
00458          10  WT-01-ENTRY-010.                                     GAS5UPD 
00459              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00460              15  WT-01-MESSAGE-TEXT-010.                          GAS5UPD 
00461                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00462                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00463                  20  FILLER          PIC X(3)  VALUE  '010'.      GAS5UPD 
00464                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00465                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00466                      'INVALID REQUEST.  THAT PF KEY HAS NO MEANINGGAS5UPD 
00467 -                    ' TO THIS PROGRAM         '.                 GAS5UPD 
00468              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00469 *----------------------------------------------------------------*GAS5UPD 
00470          10  WT-01-ENTRY-011.                                     GAS5UPD 
00471              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00472              15  WT-01-MESSAGE-TEXT-011.                          GAS5UPD 
00473                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00474                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00475                  20  FILLER          PIC X(3)  VALUE  '011'.      GAS5UPD 
00476                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00477                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00478                      'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT GAS5UPD 
00479 -                    '                         '.                 GAS5UPD 
00480              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00481 *----------------------------------------------------------------*GAS5UPD 
00482          10  WT-01-ENTRY-012.                                     GAS5UPD 
00483              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00484              15  WT-01-MESSAGE-TEXT-012.                          GAS5UPD 
00485                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00486                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00487                  20  FILLER          PIC X(3)  VALUE  '012'.      GAS5UPD 
00488                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00489                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00490                      'NO ENTRIES TO DISPLAY                       GAS5UPD 
00491 -                    '                         '.                 GAS5UPD 
00492              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00493 *----------------------------------------------------------------*GAS5UPD 
00494          10  WT-01-ENTRY-013.                                     GAS5UPD 
00495              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00496              15  WT-01-MESSAGE-TEXT-013.                          GAS5UPD 
00497                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00498                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00499                  20  FILLER          PIC X(3)  VALUE  '013'.      GAS5UPD 
00500                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00501                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00502                      'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HIGAS5UPD 
00503 -                    'T ENTER FOR ERR MSG      '.                 GAS5UPD 
00504              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00505 *----------------------------------------------------------------*GAS5UPD 
00506          10  WT-01-ENTRY-014.                                     GAS5UPD 
00507              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00508              15  WT-01-MESSAGE-TEXT-014.                          GAS5UPD 
00509                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00510                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00511                  20  FILLER          PIC X(3)  VALUE  '014'.      GAS5UPD 
00512                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00513                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00514                      'PROCESSING FROM THE TOP OF THE LIST         GAS5UPD 
00515 -                    '                         '.                 GAS5UPD 
00516              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00517 *----------------------------------------------------------------*GAS5UPD 
00518          10  WT-01-ENTRY-015.                                     GAS5UPD 
00519              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00520              15  WT-01-MESSAGE-TEXT-015.                          GAS5UPD 
00521                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00522                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00523                  20  FILLER          PIC X(3)  VALUE  '015'.      GAS5UPD 
00524                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00525                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00526                      'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDEGAS5UPD 
00527 -                    'D                        '.                 GAS5UPD 
00528              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00529 *----------------------------------------------------------------*GAS5UPD 
00530          10  WT-01-ENTRY-016.                                     GAS5UPD 
00531              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00532              15  WT-01-MESSAGE-TEXT-016.                          GAS5UPD 
00533                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00534                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00535                  20  FILLER          PIC X(3)  VALUE  '016'.      GAS5UPD 
00536                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00537                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00538                      'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUMGAS5UPD 
00539 -                    'BER OF OCCURANCES        '.                 GAS5UPD 
00540              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00541 *----------------------------------------------------------------*GAS5UPD 
00542          10  WT-01-ENTRY-017.                                     GAS5UPD 
00543              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00544              15  WT-01-MESSAGE-TEXT-017.                          GAS5UPD 
00545                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00546                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00547                  20  FILLER          PIC X(3)  VALUE  '017'.      GAS5UPD 
00548                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00549                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00550                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS5UPD 
00551 -                    'T BE CHANGED             '.                 GAS5UPD 
00552              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00553 *----------------------------------------------------------------*GAS5UPD 
00554          10  WT-01-ENTRY-018.                                     GAS5UPD 
00555              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00556              15  WT-01-MESSAGE-TEXT-018.                          GAS5UPD 
00557                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00558                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00559                  20  FILLER          PIC X(3)  VALUE  '018'.      GAS5UPD 
00560                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00561                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00562                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS5UPD 
00563 -                    'T BE MAPPED              '.                 GAS5UPD 
00564              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00565 *----------------------------------------------------------------*GAS5UPD 
00566          10  WT-01-ENTRY-019.                                     GAS5UPD 
00567              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00568              15  WT-01-MESSAGE-TEXT-019.                          GAS5UPD 
00569                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00570                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00571                  20  FILLER          PIC X(3)  VALUE  '019'.      GAS5UPD 
00572                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00573                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00574                      'THERE ARE NO MORE ENTRIES TO DISPLAY        GAS5UPD 
00575 -                    '                         '.                 GAS5UPD 
00576              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00577 *----------------------------------------------------------------*GAS5UPD 
00578          10  WT-01-ENTRY-020.                                     GAS5UPD 
00579              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00580              15  WT-01-MESSAGE-TEXT-020.                          GAS5UPD 
00581                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00582                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00583                  20  FILLER          PIC X(3)  VALUE  '020'.      GAS5UPD 
00584                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00585                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00586                      'THIS IS THE FIRST ON THE TABLE              GAS5UPD 
00587 -                    '                         '.                 GAS5UPD 
00588              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00589 *----------------------------------------------------------------*GAS5UPD 
00590          10  WT-01-ENTRY-021.                                     GAS5UPD 
00591              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00592              15  WT-01-MESSAGE-TEXT-021.                          GAS5UPD 
00593                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00594                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00595                  20  FILLER          PIC X(3)  VALUE  '021'.      GAS5UPD 
00596                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00597                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00598                      'THIS IS THE LAST ON THE TABLE               GAS5UPD 
00599 -                    '                         '.                 GAS5UPD 
00600              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00601 *----------------------------------------------------------------*GAS5UPD 
00602          10  WT-01-ENTRY-022.                                     GAS5UPD 
00603              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00604              15  WT-01-MESSAGE-TEXT-022.                          GAS5UPD 
00605                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00606                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00607                  20  FILLER          PIC X(3)  VALUE  '022'.      GAS5UPD 
00608                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00609                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00610                      'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  GAS5UPD 
00611 -                    '                         '.                 GAS5UPD 
00612              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00613 *----------------------------------------------------------------*GAS5UPD 
00614          10  WT-01-ENTRY-023.                                     GAS5UPD 
00615              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00616              15  WT-01-MESSAGE-TEXT-023.                          GAS5UPD 
00617                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00618                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00619                  20  FILLER          PIC X(3)  VALUE  '023'.      GAS5UPD 
00620                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00621                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00622                      '#IDGD HAS BEEN SUCCESSFULLY MAPPED          GAS5UPD 
00623 -                    '                         '.                 GAS5UPD 
00624              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00625 *----------------------------------------------------------------*GAS5UPD 
00626          10  WT-01-ENTRY-024.                                     GAS5UPD 
00627              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00628              15  WT-01-MESSAGE-TEXT-024.                          GAS5UPD 
00629                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00630                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00631                  20  FILLER          PIC X(3)  VALUE  '024'.      GAS5UPD 
00632                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00633                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00634                      '#IPGP HAS BEEN SUCCESSFULLY MAPPED          GAS5UPD 
00635 -                    '                         '.                 GAS5UPD 
00636              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00637 *----------------------------------------------------------------*GAS5UPD 
00638          10  WT-01-ENTRY-025.                                     GAS5UPD 
00639              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00640              15  WT-01-MESSAGE-TEXT-003.                          GAS5UPD 
00641                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00642                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00643                  20  FILLER          PIC X(3)  VALUE  '025'.      GAS5UPD 
00644                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00645                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00646                      '#IPGS HAS BEEN SUCCESSFULLY MAPPED          GAS5UPD 
00647 -                    '                         '.                 GAS5UPD 
00648              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00649 *----------------------------------------------------------------*GAS5UPD 
00650          10  WT-01-ENTRY-026.                                     GAS5UPD 
00651              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00652              15  WT-01-MESSAGE-TEXT-026.                          GAS5UPD 
00653                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00654                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00655                  20  FILLER          PIC X(3)  VALUE  '026'.      GAS5UPD 
00656                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00657                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00658                      'MAXIMUM OF 5 INTERNAL TABULARS HAS ALREADY BGAS5UPD 
00659 -                    'EEN REACHED              '.                 GAS5UPD 
00660              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00661 *----------------------------------------------------------------*GAS5UPD 
00662          10  WT-01-ENTRY-027.                                     GAS5UPD 
00663              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS5UPD 
00664              15  WT-01-MESSAGE-TEXT-027.                          GAS5UPD 
00665                  20  FILLER          PIC X(4)  VALUE  'GAS5'.     GAS5UPD 
00666                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS5UPD 
00667                  20  FILLER          PIC X(3)  VALUE  '027'.      GAS5UPD 
00668                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS5UPD 
00669                  20  FILLER          PIC X(70) VALUE              GAS5UPD 
00670                      '********** F U T U R E   U S E *************GAS5UPD 
00671 -                    '*************************'.                 GAS5UPD 
00672              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS5UPD 
00673 *----------------------------------------------------------------*GAS5UPD 
00674                                                                   GAS5UPD 
00675      05  WT-01-MESSAGE-TABLE         REDEFINES                    GAS5UPD 
00676          WT-01-MESSAGE-VALUES         OCCURS 027 TIMES            GAS5UPD 
00677                                      INDEXED BY WT-01-INDEX.      GAS5UPD 
00678          10  WT-01-ENTRY.                                         GAS5UPD 
00679              15  FILLER              PIC X(02).                   GAS5UPD 
00680              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GAS5UPD 
00681              15  FILLER              PIC X(02).                   GAS5UPD 
00682                                                                   GAS5UPD 
00683  01  WS-END                      PIC X(16)  VALUE                 GAS5UPD 
00684      '*** W/S ENDS ***'.                                          GAS5UPD 
00685 /    L I N K A G E   S E C T I O N                                GAS5UPD 
00686  LINKAGE SECTION.                                                 GAS5UPD 
00687  01  DFHCOMMAREA.                                                 GAS5UPD 
00688  COPY  G2ALCKEC.                                                  GAS5UPD 
00689  COPY  GACDACWA.                                                  GAS5UPD 
00690      05  GAS5UPD-PASSED-AREA.                                     GAS5UPD 
00691          07  LVL2-B-SW                PIC X.                      GAS5UPD 
00692          07  LVL2-F-SW                PIC X.                      GAS5UPD 
00693          07  LVL2-G-SW                PIC X.                      GAS5UPD 
00694          07  INTR-TAB-PGM-ID          PIC X(8).                   GAS5UPD 
00695          07  FILLER                   PIC X(09).                  GAS5UPD 
00696      05  DELADD-OPTION                PIC X(7).                   GAS5UPD 
00697                                                                   GAS5UPD 
00698 /*****************************************************************GAS5UPD 
00699 * W O R K F I L E   -   A L L   L E V E L   T A B U L A R   R E C GAS5UPD 
00700 ******************************************************************GAS5UPD 
00701  01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS5UPD 
00702  COPY GCIOPRM1.                                                   GAS5UPD 
00703  COPY GCWRKDCC.                                                   GAS5UPD 
00704  COPY GCTACPC.                                                    GAS5UPD 
00705 /                                                                 GAS5UPD 
00706 /*****************************************************************GAS5UPD 
00707 *    C O P Y   T A B U L A R   T A B L E   A R E A                GAS5UPD 
00708 ******************************************************************GAS5UPD 
00709  01  COPY-TABULAR-TABLE-AREA.                                     GAS5UPD 
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          GAS5UPD 
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          GAS5UPD 
00711          COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               GAS5UPD 
00712        10  COPY-SORTABLE-FLDS              PIC X(169).            GAS5UPD 
00713        10  COPY-SORT-FYI                   PIC X(003).            GAS5UPD 
00714        10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      GAS5UPD 
00715 /                                                                 GAS5UPD 
00716 /*****************************************************************GAS5UPD 
00717 * W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   GAS5UPD 
00718 ******************************************************************GAS5UPD 
00719  01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              GAS5UPD 
00720  COPY GCIOPRM2.                                                   GAS5UPD 
00721  COPY GCWRKDC2.                                                   GAS5UPD 
00722  COPY GCTIPGPC.                                                   GAS5UPD 
00723 /                                                                 GAS5UPD 
00724 /*****************************************************************GAS5UPD 
00725 * P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     GAS5UPD 
00726 ******************************************************************GAS5UPD 
00727  01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS5UPD 
00728  COPY GCIOPRMA   SUPPRESS.                                        GAS5UPD 
00729                                                                   GAS5UPD 
00730  COPY GCWRKDCA   SUPPRESS.                                        GAS5UPD 
00731                                                                   GAS5UPD 
00732  COPY GCTACP2    SUPPRESS.                                        GAS5UPD 
00733 /*****************************************************************GAS5UPD 
00734 *    M A P S E T   A R E A                                        GAS5UPD 
00735 ******************************************************************GAS5UPD 
00736      COPY GA1XSETC.                                               GAS5UPD 
00737 /    P R O C E D U R E   D I V I S I O N                          GAS5UPD 
00738  PROCEDURE DIVISION.                                              GAS5UPD 
00739                                                                   GAS5UPD 
00740 ******************************************************************GAS5UPD 
00741 * 0000  HOUSEKEEPING                                             *GAS5UPD 
00742 ******************************************************************GAS5UPD 
00743  0000-000-HOUSEKEEPING          SECTION.                          GAS5UPD 
00744  0000-010.                                                        GAS5UPD 
00745                                                                   GAS5UPD 
00746      SET ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD TO            GAS5UPD 
00747                      ACWA-WF-INTERNAL-TAB-PNTR.                   GAS5UPD 
00748                                                                   GAS5UPD 
00749      SET ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS5UPD 
00750                      ACWA-WF-ALL-LEVEL-TAB-PNTR.                  GAS5UPD 
00751                                                                   GAS5UPD 
00752      SET ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS5UPD 
00753                      ACWA-PR-ALL-LEVEL-TAB-PNTR.                  GAS5UPD 
00754                                                                   GAS5UPD 
00755      SET ADDRESS OF  GA1XI01I  TO  ACWA-MAPSET-PNTR.              GAS5UPD 
00756                                                                   GAS5UPD 
00757      MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      GAS5UPD 
00758                                                                   GAS5UPD 
00759      IF FRMNUIDI  =  'GS3A'                                       GAS5UPD 
00760         MOVE IDLINEI  TO  GROUP-SPECIFIC-ID-LINE.                 GAS5UPD 
00761      IF FRMNUIDI  =  'GC4A' OR 'GTM1'                             GAS5UPD 
00762         MOVE IDLINEI  TO  CONTRACT-ID-LINE.                       GAS5UPD 
00763      IF FRMNUIDI  =  'GC8A'                                       GAS5UPD 
00764         MOVE IDLINEI  TO  BENEFIT-PROVISION-ID-LINE.              GAS5UPD 
00765                                                                   GAS5UPD 
00766      MOVE ACP-TITLE-LINE   TO  TITLEO.                            GAS5UPD 
00767                                                                   GAS5UPD 
00768      PERFORM 1000-000-MAIN-PROCESS.                               GAS5UPD 
00769                                                                   GAS5UPD 
00770      EXEC CICS  RETURN    END-EXEC.                               GAS5UPD 
00771      GOBACK.                                                      GAS5UPD 
00772                                                                   GAS5UPD 
00773  0000-900-EXIT.                                                   GAS5UPD 
00774         EXIT.                                                     GAS5UPD 
00775 /*****************************************************************GAS5UPD 
00776 * 1000  MAIN PROCESS                                             *GAS5UPD 
00777 ******************************************************************GAS5UPD 
00778  1000-000-MAIN-PROCESS          SECTION.                          GAS5UPD 
00779  1000-010.                                                        GAS5UPD 
00780                                                                   GAS5UPD 
00781      EXEC CICS  HANDLE CONDITION                                  GAS5UPD 
00782                 MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)   END-EXEC.   GAS5UPD 
00783                                                                   GAS5UPD 
00784      MOVE  INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.      GAS5UPD 
00785                                                                   GAS5UPD 
00786      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS5UPD 
00787         DELADDI   =     'CHG/ADD'                     AND         GAS5UPD 
00788         OENTCTRI  NOT = '0000000'                                 GAS5UPD 
00789         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS5UPD 
00790                                                                   GAS5UPD 
00791      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS5UPD 
00792         DELADDI   =    'CHG/DEL'                      AND         GAS5UPD 
00793         DELOPTNI  =    'D'                                        GAS5UPD 
00794         PERFORM  2400-000-DELETE-THIS-OCCURANCE.                  GAS5UPD 
00795                                                                   GAS5UPD 
00796      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS5UPD 
00797         DELADDI   =     'CHG/DEL'                      AND        GAS5UPD 
00798         DELOPTNI  NOT = 'D'                                       GAS5UPD 
00799         MOVE SPACES  TO  ERRMSGO                                  GAS5UPD 
00800         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS5UPD 
00801                                                                   GAS5UPD 
00802      IF (EIBAID   =    DFHPF4 OR DFHPF7 OR DFHPF8 OR              GAS5UPD 
00803                        DFHPF19 OR DFHPF20 OR DFHPF16) AND         GAS5UPD 
00804         DELADDI   =    'CHG/DEL'                      AND         GAS5UPD 
00805         DELOPTNI  NOT = 'D'                                       GAS5UPD 
00806         MOVE SPACES  TO  ERRMSGO                                  GAS5UPD 
00807         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS5UPD 
00808                                                                   GAS5UPD 
00809      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS5UPD 
00810         DELADDI   =    'CHG/DEL'                      AND         GAS5UPD 
00811         DELOPTNI  NOT = 'D'                                       GAS5UPD 
00812         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS5UPD 
00813                                                                   GAS5UPD 
00814  1000-900-EXIT.   EXIT.                                           GAS5UPD 
00815                                                                   GAS5UPD 
00816 /*****************************************************************GAS5UPD 
00817 * 2200  UPDATE THIS OCCURANCE                                    *GAS5UPD 
00818 *                                                                *GAS5UPD 
00819 *    THIS ROUTINE WILL CHANGE ANY FIELD THAT THE OPERATOR HAS    *GAS5UPD 
00820 *  CHANGED, AND HAS CODE FOR THE MAINTENANCE OF THE INTERNAL     *GAS5UPD 
00821 *  TABULAR ENTRIES.                                              *GAS5UPD 
00822 ******************************************************************GAS5UPD 
00823  2200-000-UPDATE-THIS-OCCURANCE SECTION.                          GAS5UPD 
00824  2200-010.                                                        GAS5UPD 
00825                                                                   GAS5UPD 
00826      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS5UPD 
00827                                                                   GAS5UPD 
00828      IF NOT GCIO-GOOD-RETURN                                      GAS5UPD 
00829         MOVE WS-ABCODE-1PF7        TO WS-ABCODE                   GAS5UPD 
00830         MOVE WS-ABCODE-1PF7-MSG    TO WS-ABCODE-MSG               GAS5UPD 
00831         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
00832                                                                   GAS5UPD 
00833      MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   GAS5UPD 
00834      SET GAF-INDEX         TO  1.                                 GAS5UPD 
00835      MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                GAS5UPD 
00836                                                                   GAS5UPD 
00837  2200-210-FIND-RIGHT-OCCURS.                                      GAS5UPD 
00838                                                                   GAS5UPD 
00839      IF GAF-COPAY-BENEFIT-PERIOD(GAF-INDEX)  NOT = HIGH-VALUES ANDGAS5UPD 
00840         GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  NOT =                GAS5UPD 
00841                                                 ACWA-DISPLAY-LEN-7GAS5UPD 
00842      THEN                                                         GAS5UPD 
00843          IF  GAF-INDEX  <  GAF-ENTRY-COUNT                        GAS5UPD 
00844          THEN                                                     GAS5UPD 
00845              SET GAF-INDEX  UP BY  1                              GAS5UPD 
00846              GO TO 2200-210-FIND-RIGHT-OCCURS                     GAS5UPD 
00847          ELSE                                                     GAS5UPD 
00848              MOVE WS-ABCODE-1PL1        TO WS-ABCODE              GAS5UPD 
00849              MOVE WS-ABCODE-1PL1-MSG    TO WS-ABCODE-MSG          GAS5UPD 
00850              MOVE -1                    TO  MFRMSLTL              GAS5UPD 
00851              PERFORM 9800-000-ERROR-MSG-THEN-ABEND                GAS5UPD 
00852      ELSE                                                         GAS5UPD 
00853          NEXT SENTENCE.                                           GAS5UPD 
00854                                                                   GAS5UPD 
00855      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS5UPD 
00856          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS5UPD 
00857          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS5UPD 
00858          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS5UPD 
00859          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS5UPD 
00860          (IPGSOPTI  =  'MT' OR 'A')                               GAS5UPD 
00861      THEN                                                         GAS5UPD 
00862          ADD 1 TO ACWA-FIELD-CHG-CNT.                             GAS5UPD 
00863                                                                   GAS5UPD 
00864      IF DAYFACII NOT =   GAF-COPAY-DAY-FACTOR-IND (GAF-INDEX)     GAS5UPD 
00865         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00866         MOVE DAYFACII  TO  GAF-COPAY-DAY-FACTOR-IND (GAF-INDEX)   GAS5UPD 
00867      END-IF.                                                      GAS5UPD 
00868                                                                   GAS5UPD 
00869      IF COPAYINI  NOT =  GAF-COPAY-CO-PAY-IND (GAF-INDEX)         GAS5UPD 
00870         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00871         MOVE COPAYINI  TO  GAF-COPAY-CO-PAY-IND (GAF-INDEX)       GAS5UPD 
00872      END-IF.                                                      GAS5UPD 
00873                                                                   GAS5UPD 
00874      IF DEFINTNI NOT =    GAF-COPAY-DEFINITION (GAF-INDEX)        GAS5UPD 
00875         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00876         MOVE DEFINTNI  TO  GAF-COPAY-DEFINITION (GAF-INDEX)       GAS5UPD 
00877      END-IF.                                                      GAS5UPD 
00878                                                                   GAS5UPD 
00879      IF MANAPLII  NOT =  GAF-COPAY-MANDATORY-IND (GAF-INDEX)      GAS5UPD 
00880         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00881         MOVE MANAPLII TO GAF-COPAY-MANDATORY-IND (GAF-INDEX)      GAS5UPD 
00882      END-IF.                                                      GAS5UPD 
00883                                                                   GAS5UPD 
00884      IF TIMEDOLI  NOT =  GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)    GAS5UPD 
00885         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00886         MOVE TIMEDOLI TO GAF-COPAY-TIME-DOLLAR-IND (GAF-INDEX)    GAS5UPD 
00887      END-IF.                                                      GAS5UPD 
00888                                                                   GAS5UPD 
00889      IF CSTCONTI  NOT =   GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX)  GAS5UPD 
00890         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00891         MOVE CSTCONTI  TO  GAF-COPAY-COST-CONTAIN-IND (GAF-INDEX) GAS5UPD 
00892      END-IF.                                                      GAS5UPD 
00893                                                                   GAS5UPD 
00894      IF PERIODI  NOT =    GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)    GAS5UPD 
00895         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00896         MOVE PERIODI   TO  GAF-COPAY-BENEFIT-PERIOD (GAF-INDEX)   GAS5UPD 
00897      END-IF.                                                      GAS5UPD 
00898                                                                   GAS5UPD 
00899      IF PERTQALI  NOT =   GAF-COPAY-BEN-PER-TIME-QUAL (GAF-INDEX) GAS5UPD 
00900         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00901         MOVE PERTQALI  TO GAF-COPAY-BEN-PER-TIME-QUAL (GAF-INDEX) GAS5UPD 
00902      END-IF.                                                      GAS5UPD 
00903                                                                   GAS5UPD 
00904      IF FAMINDII  NOT =   GAF-COPAY-FAM-OR-INDIV (GAF-INDEX)      GAS5UPD 
00905         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00906         MOVE FAMINDII  TO  GAF-COPAY-FAM-OR-INDIV (GAF-INDEX)     GAS5UPD 
00907      END-IF.                                                      GAS5UPD 
00908                                                                   GAS5UPD 
00909      IF PLCTRMTI  NOT =   GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)GAS5UPD 
00910         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00911         MOVE PLCTRMTI  TO GAF-COPAY-PLACE-OF-TREATMENT(GAF-INDEX) GAS5UPD 
00912      END-IF.                                                      GAS5UPD 
00913                                                                   GAS5UPD 
00914      IF SRVGRUPI  NOT =   GAF-COPAY-SERVICE-GROUP (GAF-INDEX)     GAS5UPD 
00915         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00916         MOVE SRVGRUPI  TO GAF-COPAY-SERVICE-GROUP (GAF-INDEX)     GAS5UPD 
00917      END-IF.                                                      GAS5UPD 
00918                                                                   GAS5UPD 
00919      MOVE PRTIMEFI   TO ACWA-DISPLAY-LEN-3-X.                     GAS5UPD 
00920      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS5UPD 
00921                           GAF-COPAY-BEN-PER-TIME-FCTR (GAF-INDEX) GAS5UPD 
00922         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00923         MOVE ACWA-DISPLAY-LEN-3                                   GAS5UPD 
00924                       TO GAF-COPAY-BEN-PER-TIME-FCTR (GAF-INDEX)  GAS5UPD 
00925      END-IF.                                                      GAS5UPD 
00926                                                                   GAS5UPD 
00927      MOVE AGELIMLI   TO ACWA-DISPLAY-LEN-3-X.                     GAS5UPD 
00928      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS5UPD 
00929                           GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)    GAS5UPD 
00930         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00931         MOVE ACWA-DISPLAY-LEN-3                                   GAS5UPD 
00932                        TO GAF-COPAY-AGE-LIMIT-FROM (GAF-INDEX)    GAS5UPD 
00933      END-IF.                                                      GAS5UPD 
00934                                                                   GAS5UPD 
00935      MOVE AGELIMHI   TO ACWA-DISPLAY-LEN-3-X.                     GAS5UPD 
00936      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS5UPD 
00937                           GAF-COPAY-AGE-LIMIT-TO  (GAF-INDEX)     GAS5UPD 
00938         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
00939         MOVE ACWA-DISPLAY-LEN-3                                   GAS5UPD 
00940                        TO GAF-COPAY-AGE-LIMIT-TO  (GAF-INDEX)     GAS5UPD 
00941      END-IF.                                                      GAS5UPD 
00942                                                                   GAS5UPD 
           IF BISNDINI  NOT =  GAF-COPAY-BISCENDING-IND (GAF-INDEX)             
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE BISNDINI TO GAF-COPAY-BISCENDING-IND (GAF-INDEX).            
                                                                                
           IF ASCDSCDI  NOT =  GAF-COPAY-ASCEND-DESCEND-IND (GAF-INDEX)         
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE ASCDSCDI TO GAF-COPAY-ASCEND-DESCEND-IND (GAF-INDEX).        
                                                                                
      **P21595 CHANGES STARTS                                                   
           IF BENTYPI   NOT =  GAF-COPAY-BEN-TYPE           (GAF-INDEX)         
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE BENTYPI  TO GAF-COPAY-BEN-TYPE           (GAF-INDEX).        
                                                                                
           IF TIERCDI   NOT =  GAF-COPAY-TIER-CODE          (GAF-INDEX)         
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE TIERCDI  TO GAF-COPAY-TIER-CODE          (GAF-INDEX).        
                                                                                
           IF TIERLVI   NOT =  GAF-COPAY-TIER-LVL           (GAF-INDEX)         
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE TIERLVI  TO GAF-COPAY-TIER-LVL           (GAF-INDEX).        
      **P21595 CHANGES ENDS                                                     
                                                                                
00943      IF FEAKINDI  NOT =  GAF-COPAY-FEAK-IND        (GAF-INDEX)    GAS5UPD 
00944         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00945         MOVE FEAKINDI TO GAF-COPAY-FEAK-IND        (GAF-INDEX)    GAS5UPD 
00946      END-IF.                                                      GAS5UPD 
00947                                                                   GAS5UPD 
00948      IF ACCUMIDI  NOT =  GAF-COPAY-ACCUMID         (GAF-INDEX)    GAS5UPD 
00949         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00950         MOVE ACCUMIDI TO GAF-COPAY-ACCUMID         (GAF-INDEX)    GAS5UPD 
00951      END-IF.                                                      GAS5UPD 
00952                                                                   GAS5UPD 
00953      IF CAPINDI   NOT =  GAF-COPAY-COMB-APPLIED-IND (GAF-INDEX)   GAS5UPD 
00954         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00955         MOVE CAPINDI  TO GAF-COPAY-COMB-APPLIED-IND (GAF-INDEX)   GAS5UPD 
00956      END-IF.                                                      GAS5UPD 
00957                                                                   GAS5UPD 
00958      IF SABDINDI  NOT =  GAF-COPAY-SEL-ADDL-BEN-DET (GAF-INDEX)   GAS5UPD 
00959         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00960         MOVE SABDINDI TO GAF-COPAY-SEL-ADDL-BEN-DET (GAF-INDEX)   GAS5UPD 
00961      END-IF.                                                      GAS5UPD 
00962                                                                   GAS5UPD 
00963      IF AGEQLLI   NOT =  GAF-COPAY-AGE-QUAL-IND-FROM(GAF-INDEX)   GAS5UPD 
00964         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00965         MOVE AGEQLLI  TO                                          GAS5UPD 
00966                         GAF-COPAY-AGE-QUAL-IND-FROM(GAF-INDEX)    GAS5UPD 
00967      END-IF.                                                      GAS5UPD 
00968                                                                   GAS5UPD 
00969      IF AGEQLHI   NOT =  GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX)    GAS5UPD 
00970         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00971         MOVE AGEQLHI  TO                                          GAS5UPD 
00972                         GAF-COPAY-AGE-QUAL-IND-TO (GAF-INDEX)     GAS5UPD 
00973      END-IF.                                                      GAS5UPD 
00974                                                                   GAS5UPD 
00975      IF RELPINDI  NOT =  GAF-COPAY-RELATIONSHIP-IND (GAF-INDEX)   GAS5UPD 
00976         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00977         MOVE RELPINDI TO                                          GAS5UPD 
00978                         GAF-COPAY-RELATIONSHIP-IND (GAF-INDEX)    GAS5UPD 
00979      END-IF.                                                      GAS5UPD 
00980                                                                   GAS5UPD 
00981      IF CLMLVLII NOT =   GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX)GAS5UPD 
00982         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00983         MOVE CLMLVLII TO GAF-COPAY-CLAIM-LVL-ACCUM-IND(GAF-INDEX) GAS5UPD 
00984      END-IF.                                                      GAS5UPD 
00985                                                                   GAS5UPD 
00986      MOVE INTRVALI    TO ACWA-DISPLAY-LEN-3-X.                    GAS5UPD 
00987      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS5UPD 
00988                          GAF-COPAY-INTERVAL-TIME-FCTR (GAF-INDEX) GAS5UPD 
00989         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00990         MOVE ACWA-DISPLAY-LEN-3 TO                                GAS5UPD 
00991                         GAF-COPAY-INTERVAL-TIME-FCTR (GAF-INDEX)  GAS5UPD 
00992      END-IF.                                                      GAS5UPD 
00993                                                                   GAS5UPD 
00994      IF INTTYPEI  NOT =  GAF-COPAY-INTERVAL-TYPE (GAF-INDEX)      GAS5UPD 
00995         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
00996         MOVE INTTYPEI  TO  GAF-COPAY-INTERVAL-TYPE (GAF-INDEX)    GAS5UPD 
00997      END-IF.                                                      GAS5UPD 
00998                                                                   GAS5UPD 
00999      IF LOBI NOT =       GAF-COPAY-L-O-B  (GAF-INDEX)             GAS5UPD 
01000         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
01001         MOVE LOBI     TO GAF-COPAY-L-O-B (GAF-INDEX)              GAS5UPD 
01002      END-IF.                                                      GAS5UPD 
01003                                                                   GAS5UPD 
01004      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS5UPD 
01005          MOVE ACWA-VAL-LIM-SCREEN TO ACWA-VALUE-LIMIT-9-9         GAS5UPD 
01006      ELSE                                                         GAS5UPD 
01007          IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                GAS5UPD 
01008              ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG'                   GAS5UPD 
01009              MOVE -1                  TO ACWA-VALUE-LIMIT-9-9     GAS5UPD 
01010          ELSE                                                     GAS5UPD 
01007          IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                GAS5UPD 
01008              ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL'                   GAS5UPD 
01009              MOVE -2                  TO ACWA-VALUE-LIMIT-9-9     GAS5UPD 
01010          ELSE                                                     GAS5UPD 
01011              MOVE ACWA-VAL-LIM-SCREEN-7 TO ACWA-VALUE-LIMIT-7     GAS5UPD 
01012              MOVE ACWA-VAL-LIM-SCREEN-2 TO ACWA-VALUE-LIMIT-2.    GAS5UPD 
01013                                                                   GAS5UPD 
01014      IF ACWA-VALUE-LIMIT-9 NOT = GAF-COPAY-VALUE-LIMIT (GAF-INDEX)GAS5UPD 
01015         PERFORM 2600-000-PROCESS-VAL-LIMIT                        GAS5UPD 
01016      END-IF.                                                      GAS5UPD 
01017                                                                   GAS5UPD 
01018      IF ACWA-VALUE-LIMIT-9 NOT = GAF-COPAY-VALUE-LIMIT (GAF-INDEX)GAS5UPD 
01019         ADD 1                  TO ACWA-FIELD-CHG-CNT              GAS5UPD 
01020        MOVE ACWA-VALUE-LIMIT-9 TO GAF-COPAY-VALUE-LIMIT(GAF-INDEX)GAS5UPD 
01021      END-IF.                                                      GAS5UPD 
01022                                                                   GAS5UPD 
01023      IF BENVLQLI  NOT =   GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX)   GAS5UPD 
01024         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01025         MOVE BENVLQLI  TO  GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX)  GAS5UPD 
01026      END-IF.                                                      GAS5UPD 
01027                                                                   GAS5UPD 
01028      MOVE NEWVALUI     TO ACWA-DISPLAY-LEN-5-X.                   GAS5UPD 
01029      IF  ACWA-DISPLAY-LEN-5 NOT =                                 GAS5UPD 
01030                       GAF-COPAY-INTERVAL-OVRD-VALUE (GAF-INDEX)   GAS5UPD 
01031          ADD 1     TO ACWA-FIELD-CHG-CNT                          GAS5UPD 
01032          MOVE ACWA-DISPLAY-LEN-5                                  GAS5UPD 
01033                    TO  GAF-COPAY-INTERVAL-OVRD-VALUE (GAF-INDEX)  GAS5UPD 
01034      END-IF.                                                      GAS5UPD 
01035                                                                   GAS5UPD 
01036      IF OVRDINDI  NOT =   GAF-COPAY-INTERVAL-OVRD-IND (GAF-INDEX) GAS5UPD 
01037         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01038         MOVE OVRDINDI  TO  GAF-COPAY-INTERVAL-OVRD-IND (GAF-INDEX)GAS5UPD 
01039      END-IF.                                                      GAS5UPD 
01040                                                                   GAS5UPD 
01041      IF FYIVALI   NOT =  GAF-COPAY-FYI-VALUE (GAF-INDEX)          GAS5UPD 
01042         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
01043         MOVE FYIVALI  TO  GAF-COPAY-FYI-VALUE (GAF-INDEX)         GAS5UPD 
01044      END-IF.                                                      GAS5UPD 
01045                                                                   GAS5UPD 
01046      IF INTDESKI  =  GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX)    GAS5UPD 
01047         MOVE SPACE  TO  WS-INT-TAB-CHANGE-INDICATOR               GAS5UPD 
01048      ELSE                                                         GAS5UPD 
01049         ADD 1   TO  ACWA-FIELD-CHG-CNT                            GAS5UPD 
01050         IF INTDESKI  =  IDPRODI                                   GAS5UPD 
01051            MOVE 'NP'   TO  WS-INT-TAB-CHANGE-INDICATOR            GAS5UPD 
01052            MOVE INTDESKI  TO                                      GAS5UPD 
01053                          GAF-COPAY-INTERNAL-DESCRIPTOR(GAF-INDEX) GAS5UPD 
01054         ELSE                                                      GAS5UPD 
01055            IF IDPRODI  = GAF-COPAY-INTERNAL-DESCRIPTOR(GAF-INDEX) GAS5UPD 
01056               MOVE 'PN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS5UPD 
01057               MOVE INTDESKI  TO                                   GAS5UPD 
01058                          GAF-COPAY-INTERNAL-DESCRIPTOR(GAF-INDEX) GAS5UPD 
01059            ELSE                                                   GAS5UPD 
01060               MOVE 'NN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS5UPD 
01061               MOVE INTDESKI  TO                                   GAS5UPD 
01062                           GAF-COPAY-INTERNAL-DESCRIPTOR(GAF-INDEX)GAS5UPD 
01063            END-IF.                                                GAS5UPD 
01064                                                                   GAS5UPD 
01065                                                                   GAS5UPD 
01066      IF CONDALLI  NOT =   GAF-COND-ALL-BIT (GAF-INDEX)            GAS5UPD 
01067         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01068         MOVE CONDALLI  TO  GAF-COND-ALL-BIT (GAF-INDEX)           GAS5UPD 
01069      END-IF.                                                      GAS5UPD 
01070                                                                   GAS5UPD 
01071      IF CONDEXCI  NOT =   GAF-COND-EXCLUSION-BIT (GAF-INDEX)      GAS5UPD 
01072         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01073         MOVE CONDEXCI  TO  GAF-COND-EXCLUSION-BIT (GAF-INDEX)     GAS5UPD 
01074      END-IF.                                                      GAS5UPD 
01075                                                                   GAS5UPD 
01076      IF CONDICDI  NOT =   GAF-COND-ICD-BIT (GAF-INDEX)            GAS5UPD 
01077         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01078         MOVE CONDICDI  TO  GAF-COND-ICD-BIT (GAF-INDEX)           GAS5UPD 
01079      END-IF.                                                      GAS5UPD 
01080                                                                   GAS5UPD 
01081      IF CONDTABI  NOT =   GAF-COND-TB-BIT (GAF-INDEX)             GAS5UPD 
01082         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01083         MOVE CONDTABI  TO GAF-COND-TB-BIT (GAF-INDEX)             GAS5UPD 
01084      END-IF.                                                      GAS5UPD 
01085                                                                   GAS5UPD 
01086      IF CONDMENI  NOT =   GAF-COND-MENTAL-BIT (GAF-INDEX)         GAS5UPD 
01087         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01088         MOVE CONDMENI  TO GAF-COND-MENTAL-BIT (GAF-INDEX)         GAS5UPD 
01089      END-IF.                                                      GAS5UPD 
01090                                                                   GAS5UPD 
01091      IF CONDDRGI  NOT =   GAF-COND-DRUG-BIT (GAF-INDEX)           GAS5UPD 
01092         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01093         MOVE CONDDRGI  TO GAF-COND-DRUG-BIT (GAF-INDEX)           GAS5UPD 
01094      END-IF.                                                      GAS5UPD 
01095                                                                   GAS5UPD 
01096      IF CONDALCI  NOT =   GAF-COND-ALCOHOL-BIT (GAF-INDEX)        GAS5UPD 
01097         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01098         MOVE CONDALCI  TO GAF-COND-ALCOHOL-BIT (GAF-INDEX)        GAS5UPD 
01099      END-IF.                                                      GAS5UPD 
01100                                                                   GAS5UPD 
01101      IF CONDOBCI  NOT =   GAF-COND-OB-COMP-BIT (GAF-INDEX)        GAS5UPD 
01102         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01103         MOVE CONDOBCI  TO GAF-COND-OB-COMP-BIT (GAF-INDEX)        GAS5UPD 
01104      END-IF.                                                      GAS5UPD 
01105                                                                   GAS5UPD 
01106      IF CONDOBNI  NOT =   GAF-COND-OB-NORM-BIT (GAF-INDEX)        GAS5UPD 
01107         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01108         MOVE CONDOBNI  TO GAF-COND-OB-NORM-BIT (GAF-INDEX)        GAS5UPD 
01109      END-IF.                                                      GAS5UPD 
01110                                                                   GAS5UPD 
01111      IF CONDMALI  NOT =   GAF-COND-MALIGNANCY-BIT (GAF-INDEX)     GAS5UPD 
01112         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS5UPD 
01113         MOVE CONDMALI  TO GAF-COND-MALIGNANCY-BIT (GAF-INDEX)     GAS5UPD 
01114      END-IF.                                                      GAS5UPD 
01115                                                                   GAS5UPD 
01116      IF CONDCARI  NOT =  GAF-COND-CARDIAC-DISEASE-BIT (GAF-INDEX) GAS5UPD 
01117         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
01118        MOVE CONDCARI  TO GAF-COND-CARDIAC-DISEASE-BIT (GAF-INDEX) GAS5UPD 
01119      END-IF.                                                      GAS5UPD 
01120                                                                   GAS5UPD 
01121      IF CONDOBSI  NOT =  GAF-COND-OBESITY-BIT (GAF-INDEX)         GAS5UPD 
01122         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
01123         MOVE CONDOBSI TO GAF-COND-OBESITY-BIT (GAF-INDEX)         GAS5UPD 
01124      END-IF.                                                      GAS5UPD 
01125                                                                   GAS5UPD 
01126      IF CONDKDYI  NOT =  GAF-COND-KIDNEY-DISEASE-BIT (GAF-INDEX)  GAS5UPD 
01127         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS5UPD 
01128         MOVE CONDKDYI TO GAF-COND-KIDNEY-DISEASE-BIT (GAF-INDEX)  GAS5UPD 
01129      END-IF.                                                      GAS5UPD 
01130                                                                   GAS5UPD 
01131      IF CONDACCI  NOT  =   GAF-COND-ACCIDENT-BIT (GAF-INDEX)      GAS5UPD 
01132         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01133         MOVE CONDACCI  TO  GAF-COND-ACCIDENT-BIT (GAF-INDEX)      GAS5UPD 
01134      END-IF.                                                      GAS5UPD 
01135                                                                   GAS5UPD 
01136      IF CONDPECI  NOT  =  GAF-COND-PRE-EXIST-BIT (GAF-INDEX)      GAS5UPD 
01137         ADD  1        TO  ACWA-FIELD-CHG-CNT                      GAS5UPD 
01138         MOVE CONDPECI TO  GAF-COND-PRE-EXIST-BIT (GAF-INDEX)      GAS5UPD 
01139      END-IF.                                                      GAS5UPD 
01140                                                                   GAS5UPD 
01141      IF CONDNEMI  NOT  =   GAF-COND-NON-EMER-BIT (GAF-INDEX)      GAS5UPD 
01142         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01143         MOVE CONDNEMI  TO  GAF-COND-NON-EMER-BIT (GAF-INDEX)      GAS5UPD 
01144      END-IF.                                                      GAS5UPD 
01145                                                                   GAS5UPD 
01146      IF CONDSUII  NOT  =   GAF-COND-SUICIDE-BIT  (GAF-INDEX)      GAS5UPD 
01147         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01148         MOVE CONDSUII  TO  GAF-COND-SUICIDE-BIT  (GAF-INDEX)      GAS5UPD 
01149      END-IF.                                                      GAS5UPD 
01150                                                                   GAS5UPD 
01151      IF CONDTMJI  NOT  =   GAF-COND-TMJ-BIT      (GAF-INDEX)      GAS5UPD 
01152         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01153         MOVE CONDTMJI  TO  GAF-COND-TMJ-BIT      (GAF-INDEX)      GAS5UPD 
01154      END-IF.                                                      GAS5UPD 
01155                                                                   GAS5UPD 
01156      IF CONDINFI  NOT  =   GAF-COND-INF-BIT      (GAF-INDEX)      GAS5UPD 
01157         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01158         MOVE CONDINFI  TO  GAF-COND-INF-BIT      (GAF-INDEX)      GAS5UPD 
01159      END-IF.                                                      GAS5UPD 
01160                                                                   GAS5UPD 
01161      IF CONDLIFI  NOT  =   GAF-COND-LIFE-THREAT-BIT (GAF-INDEX)   GAS5UPD 
01162         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01163         MOVE CONDLIFI  TO  GAF-COND-LIFE-THREAT-BIT  (GAF-INDEX)  GAS5UPD 
01164      END-IF.                                                      GAS5UPD 
01165                                                                   GAS5UPD 
01166      IF CONDEMCI  NOT  =   GAF-COND-EMER-MED-BIT    (GAF-INDEX)   GAS5UPD 
01167         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01168         MOVE CONDEMCI  TO  GAF-COND-EMER-MED-BIT     (GAF-INDEX)  GAS5UPD 
01169      END-IF.                                                      GAS5UPD 
01170                                                                   GAS5UPD 
01171      IF CONDEACI  NOT  =   GAF-COND-EMER-ACC-BIT    (GAF-INDEX)   GAS5UPD 
01172         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01173         MOVE CONDEACI  TO  GAF-COND-EMER-ACC-BIT     (GAF-INDEX)  GAS5UPD 
01174      END-IF.                                                      GAS5UPD 
01175                                                                   GAS5UPD 
01176      IF CONDSMII  NOT  = GAF-COND-SER-MEN-ILL-BIT   (GAF-INDEX)   GAS5UPD 
01177         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01178         MOVE CONDSMII  TO GAF-COND-SER-MEN-ILL-BIT   (GAF-INDEX)  GAS5UPD 
01179      END-IF.                                                      GAS5UPD 
01180                                                                   GAS5UPD 
01181      IF CONDNSMI  NOT  = GAF-COND-NON-SER-MEN-ILL-BIT (GAF-INDEX) GAS5UPD 
01182         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS5UPD 
01183         MOVE CONDNSMI  TO GAF-COND-NON-SER-MEN-ILL-BIT (GAF-INDEX)GAS5UPD 
01184      END-IF.                                                      GAS5UPD 
01185                                                                   GAS5UPD 
01186      IF FRMNUIDI  =  'GC8A'                                       GAS5UPD 
01187         MOVE GCIO-WRK-TABULAR-PROVISION  TO                       GAS5UPD 
01188                                     GCIO-WRK-BENEFIT-PROVISION.   GAS5UPD 
01189                                                                   GAS5UPD 
01190      PERFORM  5000-000-READ-PROD-ALL-LVL-TAB.                     GAS5UPD 
01191      SEARCH GAF2-ENTRY                                            GAS5UPD 
01192            VARYING GAF2-INDEX                                     GAS5UPD 
01193            WHEN                                                   GAS5UPD 
01194               GAF2-INDEX NOT <  GAF2-ENTRY-COUNT  OR              GAS5UPD 
01195               GAF2-OCCURS-ENTRY-COUNTER(GAF2-INDEX)  =            GAS5UPD 
01196                              GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  GAS5UPD 
01197               NEXT SENTENCE.                                      GAS5UPD 
01198                                                                   GAS5UPD 
01199      IF GAF2-INDEX <  GAF2-ENTRY-COUNT  AND                       GAS5UPD 
01200            GAF2-OCCURS-ENTRY-COUNTER(GAF2-INDEX)  =               GAS5UPD 
01201                              GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  GAS5UPD 
01202            MOVE 'N'      TO  WS-NEW-OCCR-ON-WF                    GAS5UPD 
01203      ELSE                                                         GAS5UPD 
01204            MOVE 'Y'      TO WS-NEW-OCCR-ON-WF.                    GAS5UPD 
01205                                                                   GAS5UPD 
01206      IF ACWA-INTERNAL-TAB-CHANGE-ONLY                             GAS5UPD 
01207         GO TO 2200-260-CHANGE-INTERNAL-TAB.                       GAS5UPD 
01208                                                                   GAS5UPD 
01209 *** CHECK LVL2-B-SWITCH                                           GAS5UPD 
01210      IF EIBAID    =       DFHENTER  AND                           GAS5UPD 
01211         DELADDI   =      'CHG/ADD'  AND                           GAS5UPD 
01212         OENTCTRI  NOT =  '0000000'  AND                           GAS5UPD 
01213         ACWA-NO-CHANGE-FOUND                                      GAS5UPD 
01214         MOVE 'Y'   TO  LVL2-B-SW                                  GAS5UPD 
01215         PERFORM 3100-RLSE-RU-GAF-REC                              GAS5UPD 
01216         GO  TO  2200-900-EXIT.                                    GAS5UPD 
01217                                                                   GAS5UPD 
01218      IF  EIBAID  =  DFHENTER       AND                            GAS5UPD 
01219          ACWA-SCREEN-HAS-NO-ERRORS AND                            GAS5UPD 
01220          GCVI-TABLE-SW = 'N'                                      GAS5UPD 
01221          IF  ACWA-NO-CHANGE-FOUND                                 GAS5UPD 
01222              SET  WT-01-INDEX                     TO +11          GAS5UPD 
01223              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS5UPD 
01224              PERFORM 7900-000-RESET-ATTRIBUTES                    GAS5UPD 
01225              MOVE -1 TO PERIODL                                   GAS5UPD 
01226              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS5UPD 
01227          ELSE                                                     GAS5UPD 
01228              SET  WT-01-INDEX                     TO +07          GAS5UPD 
01229              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS5UPD 
01230              PERFORM 9010-000-SEND-DATAONLY-RETURN.               GAS5UPD 
01231                                                                   GAS5UPD 
01232      IF (EIBAID  =  DFHPF4 OR  DFHPF16) AND                       GAS5UPD 
01233          ACWA-SCREEN-HAS-NO-ERRORS      AND                       GAS5UPD 
01234          GCVI-TABLE-SW = 'N'            AND                       GAS5UPD 
01235          ACWA-NO-CHANGE-FOUND                                     GAS5UPD 
01236      THEN                                                         GAS5UPD 
01237          SET  WT-01-INDEX                     TO +09              GAS5UPD 
01238          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS5UPD 
01239          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS5UPD 
01240          MOVE -1 TO PERIODL                                       GAS5UPD 
01241          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS5UPD 
01242                                                                   GAS5UPD 
01243      IF  EIBAID   =   DFHENTER AND                                GAS5UPD 
01244          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS5UPD 
01245          ACWA-NO-CHANGE-FOUND                                     GAS5UPD 
01246      THEN                                                         GAS5UPD 
01247          SET  WT-01-INDEX                     TO +11              GAS5UPD 
01248          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS5UPD 
01249          MOVE -1 TO PERIODL                                       GAS5UPD 
01250          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS5UPD 
01251                                                                   GAS5UPD 
01252      PERFORM 7900-000-RESET-ATTRIBUTES.                           GAS5UPD 
01253                                                                   GAS5UPD 
01254 *** LVL2-F-SWITCH                                                 GAS5UPD 
01255      IF  (EIBAID  =   DFHPF7 OR DFHPF19) AND                      GAS5UPD 
01256          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS5UPD 
01257          ACWA-NO-CHANGE-FOUND                                     GAS5UPD 
01258      THEN                                                         GAS5UPD 
01259          MOVE  'Y'   TO  LVL2-F-SW                                GAS5UPD 
01260         PERFORM 3100-RLSE-RU-GAF-REC                              GAS5UPD 
01261          GO TO  2200-900-EXIT.                                    GAS5UPD 
01262                                                                   GAS5UPD 
01263 *** LVL2-G-SWITCH                                                 GAS5UPD 
01264      IF  (EIBAID  =  DFHPF8 OR DFHPF20) AND                       GAS5UPD 
01265          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS5UPD 
01266          ACWA-NO-CHANGE-FOUND                                     GAS5UPD 
01267      THEN                                                         GAS5UPD 
01268          MOVE  'Y'   TO  LVL2-G-SW                                GAS5UPD 
01269         PERFORM 3100-RLSE-RU-GAF-REC                              GAS5UPD 
01270          GO TO  2200-900-EXIT.                                    GAS5UPD 
01271                                                                   GAS5UPD 
01272      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS5UPD 
01273         DELADDI  =  'CHG/DEL'                                     GAS5UPD 
01274         PERFORM 4600-000-UPDATE-CDE-STATUS.                       GAS5UPD 
01275                                                                   GAS5UPD 
01276      IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE OR             GAS5UPD 
01277          (IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999')) AND      GAS5UPD 
01278          (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE OR             GAS5UPD 
01279          (IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999')) AND      GAS5UPD 
01280          (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE OR             GAS5UPD 
01281          (IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999')) AND      GAS5UPD 
01282          (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE OR             GAS5UPD 
01283          (IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999')) AND      GAS5UPD 
01284          (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE OR             GAS5UPD 
01285          (IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999')) AND      GAS5UPD 
01286          (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE OR             GAS5UPD 
01287          (IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'))          GAS5UPD 
01288      THEN                                                         GAS5UPD 
01289          GO TO 2200-250-UPDATE-ALL-LVL-TAB.                       GAS5UPD 
01290                                                                   GAS5UPD 
01291      IF  IBGROPTI  =  'C' OR                                      GAS5UPD 
01292          IDGDOPTI  =  'C' OR                                      GAS5UPD 
01293          IPGNOPTI  =  'C' OR                                      GAS5UPD 
01294          IPGPOPTI  =  'C' OR                                      GAS5UPD 
01295          IPGTOPTI  =  'C' OR                                      GAS5UPD 
01296          IPGSOPTI  =  'C'                                         GAS5UPD 
01297      THEN                                                         GAS5UPD 
01298          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS5UPD 
01299                                                                   GAS5UPD 
01300      IF  (IBGROPTI  =  'MT' OR 'A') AND IBGRSLTI  =  '0000000' OR GAS5UPD 
01301          (IDGDOPTI  =  'MT' OR 'A') AND IDGDSLTI  =  '0000000' OR GAS5UPD 
01302          (IPGNOPTI  =  'MT' OR 'A') AND IPGNSLTI  =  '0000000' OR GAS5UPD 
01303          (IPGPOPTI  =  'MT' OR 'A') AND IPGPSLTI  =  '0000000' OR GAS5UPD 
01304          (IPGTOPTI  =  'MT' OR 'A') AND IPGTSLTI  =  '0000000' OR GAS5UPD 
01305          (IPGSOPTI  =  'MT' OR 'A') AND IPGSSLTI  =  '0000000'    GAS5UPD 
01306      THEN                                                         GAS5UPD 
01307          GO TO 2200-220-ADD-INTERNAL-OCCURS.                      GAS5UPD 
01308                                                                   GAS5UPD 
01309      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS5UPD 
01310          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS5UPD 
01311          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS5UPD 
01312          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS5UPD 
01313          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS5UPD 
01314          (IPGSOPTI  =  'MT' OR 'A')                               GAS5UPD 
01315      THEN                                                         GAS5UPD 
01316          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS5UPD 
01317                                                                   GAS5UPD 
01318      IF  IBGROPTI  =  'D'                                         GAS5UPD 
01319          MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID             GAS5UPD 
01320          MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO             GAS5UPD 
01321      ELSE                                                         GAS5UPD 
01322          IF  IPGNOPTI  =  'D'                                     GAS5UPD 
01323              MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID         GAS5UPD 
01324              MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO         GAS5UPD 
01325          ELSE                                                     GAS5UPD 
01326              IF  IPGTOPTI  =  'D'                                 GAS5UPD 
01327                  MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS5UPD 
01328                  MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS5UPD 
01329              ELSE                                                 GAS5UPD 
01330              IF  IPGSOPTI  =  'D'                                 GAS5UPD 
01331                  MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS5UPD 
01332                  MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS5UPD 
01333              ELSE                                                 GAS5UPD 
01334              IF  IDGDOPTI  =  'D'                                 GAS5UPD 
01335                  MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS5UPD 
01336                  MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS5UPD 
01337              ELSE                                                 GAS5UPD 
01338              IF  IPGPOPTI  =  'D'                                 GAS5UPD 
01339                  MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS5UPD 
01340                  MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.    GAS5UPD 
01341                                                                   GAS5UPD 
01342                                                                   GAS5UPD 
01343      IF    GAF-COPAY-INTL-TAB-1 (GAF-INDEX)                       GAS5UPD 
01344          = GCIO-WRK-TABULAR-PROVISION                             GAS5UPD 
01345          MOVE GAF-COPAY-INTL-TAB-2 (GAF-INDEX)                    GAS5UPD 
01346            TO GAF-COPAY-INTL-TAB-1 (GAF-INDEX)                    GAS5UPD 
01347          MOVE GAF-COPAY-INTL-TAB-3 (GAF-INDEX)                    GAS5UPD 
01348            TO GAF-COPAY-INTL-TAB-2 (GAF-INDEX)                    GAS5UPD 
01349          MOVE GAF-COPAY-INTL-TAB-4 (GAF-INDEX)                    GAS5UPD 
01350            TO GAF-COPAY-INTL-TAB-3 (GAF-INDEX)                    GAS5UPD 
01351          MOVE GAF-COPAY-INTL-TAB-5 (GAF-INDEX)                    GAS5UPD 
01352            TO GAF-COPAY-INTL-TAB-4 (GAF-INDEX)                    GAS5UPD 
01353 ******** MOVE HIGH-VALUES                                         GAS5UPD 
01354 ********   TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)                    GAS5UPD 
01355          IF GAF-COPAY-INTL-TAB-4 (GAF-INDEX)                      GAS5UPD 
01356                       = HIGH-VALUES OR WS-SPACES-ZEROS            GAS5UPD 
01357             MOVE WS-SPACES-ZEROS                                  GAS5UPD 
01358               TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)                 GAS5UPD 
01359          ELSE                                                     GAS5UPD 
01360             MOVE HIGH-VALUES                                      GAS5UPD 
01361               TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)                 GAS5UPD 
01362          END-IF                                                   GAS5UPD 
01363      ELSE                                                         GAS5UPD 
01364          IF   GAF-COPAY-INTL-TAB-2 (GAF-INDEX)                    GAS5UPD 
01365             = GCIO-WRK-TABULAR-PROVISION                          GAS5UPD 
01366             MOVE GAF-COPAY-INTL-TAB-3 (GAF-INDEX)                 GAS5UPD 
01367               TO GAF-COPAY-INTL-TAB-2 (GAF-INDEX)                 GAS5UPD 
01368             MOVE GAF-COPAY-INTL-TAB-4 (GAF-INDEX)                 GAS5UPD 
01369               TO GAF-COPAY-INTL-TAB-3 (GAF-INDEX)                 GAS5UPD 
01370             MOVE GAF-COPAY-INTL-TAB-5 (GAF-INDEX)                 GAS5UPD 
01371               TO GAF-COPAY-INTL-TAB-4 (GAF-INDEX)                 GAS5UPD 
01372 *********** MOVE HIGH-VALUES                                      GAS5UPD 
01373 ***********   TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)                 GAS5UPD 
01374             IF GAF-COPAY-INTL-TAB-4 (GAF-INDEX)                   GAS5UPD 
01375                          = HIGH-VALUES OR WS-SPACES-ZEROS         GAS5UPD 
01376                MOVE WS-SPACES-ZEROS                               GAS5UPD 
01377                  TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)              GAS5UPD 
01378             ELSE                                                  GAS5UPD 
01379                MOVE HIGH-VALUES                                   GAS5UPD 
01380                  TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)              GAS5UPD 
01381             END-IF                                                GAS5UPD 
01382         ELSE                                                      GAS5UPD 
01383             IF    GAF-COPAY-INTL-TAB-3 (GAF-INDEX)                GAS5UPD 
01384                 = GCIO-WRK-TABULAR-PROVISION                      GAS5UPD 
01385                 MOVE GAF-COPAY-INTL-TAB-4 (GAF-INDEX)             GAS5UPD 
01386                   TO GAF-COPAY-INTL-TAB-3 (GAF-INDEX)             GAS5UPD 
01387                 MOVE GAF-COPAY-INTL-TAB-5 (GAF-INDEX)             GAS5UPD 
01388                   TO GAF-COPAY-INTL-TAB-4 (GAF-INDEX)             GAS5UPD 
01389 *************** MOVE HIGH-VALUES                                  GAS5UPD 
01390 ***************   TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)             GAS5UPD 
01391                 IF GAF-COPAY-INTL-TAB-4 (GAF-INDEX)               GAS5UPD 
01392                              = HIGH-VALUES OR WS-SPACES-ZEROS     GAS5UPD 
01393                    MOVE WS-SPACES-ZEROS                           GAS5UPD 
01394                      TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)          GAS5UPD 
01395                 ELSE                                              GAS5UPD 
01396                    MOVE HIGH-VALUES                               GAS5UPD 
01397                      TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)          GAS5UPD 
01398                 END-IF                                            GAS5UPD 
01399             ELSE                                                  GAS5UPD 
01400                 IF    GAF-COPAY-INTL-TAB-4 (GAF-INDEX)            GAS5UPD 
01401                     = GCIO-WRK-TABULAR-PROVISION                  GAS5UPD 
01402                     MOVE GAF-COPAY-INTL-TAB-5 (GAF-INDEX)         GAS5UPD 
01403                       TO GAF-COPAY-INTL-TAB-4 (GAF-INDEX)         GAS5UPD 
01404 ******************* MOVE HIGH-VALUES                              GAS5UPD 
01405 *******************   TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)         GAS5UPD 
01406                     IF GAF-COPAY-INTL-TAB-4 (GAF-INDEX)           GAS5UPD 
01407                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS5UPD 
01408                        MOVE WS-SPACES-ZEROS                       GAS5UPD 
01409                          TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)      GAS5UPD 
01410                     ELSE                                          GAS5UPD 
01411                        MOVE HIGH-VALUES                           GAS5UPD 
01412                          TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)      GAS5UPD 
01413                     END-IF                                        GAS5UPD 
01414             ELSE                                                  GAS5UPD 
01415                 IF    GAF-COPAY-INTL-TAB-5 (GAF-INDEX)            GAS5UPD 
01416                     = GCIO-WRK-TABULAR-PROVISION                  GAS5UPD 
01417 ******************* MOVE HIGH-VALUES                              GAS5UPD 
01418 *******************   TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)         GAS5UPD 
01419                     IF GAF-COPAY-INTL-TAB-4 (GAF-INDEX)           GAS5UPD 
01420                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS5UPD 
01421                        MOVE WS-SPACES-ZEROS                       GAS5UPD 
01422                          TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)      GAS5UPD 
01423                     ELSE                                          GAS5UPD 
01424                        MOVE HIGH-VALUES                           GAS5UPD 
01425                          TO GAF-COPAY-INTL-TAB-5 (GAF-INDEX)      GAS5UPD 
01426                     END-IF                                        GAS5UPD 
01427                 ELSE                                              GAS5UPD 
01428                     MOVE WS-ABCODE-1PL2        TO WS-ABCODE       GAS5UPD 
01429                     MOVE WS-ABCODE-1PL2-MSG    TO WS-ABCODE-MSG   GAS5UPD 
01430                     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.        GAS5UPD 
01431                                                                   GAS5UPD 
01432 **** SUBTRACT  1  FROM  GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX).   GAS5UPD 
01433      IF GAF-COPAY-INTL-TAB-5 (GAF-INDEX) = HIGH-VALUES            GAS5UPD 
01434         NEXT SENTENCE                                             GAS5UPD 
01435      ELSE                                                         GAS5UPD 
01436         SUBTRACT  1  FROM  GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX).GAS5UPD 
01437                                                                   GAS5UPD 
01438      GO TO 2200-250-UPDATE-ALL-LVL-TAB.                           GAS5UPD 
01439                                                                   GAS5UPD 
01440                                                                   GAS5UPD 
01441  2200-220-ADD-INTERNAL-OCCURS.                                    GAS5UPD 
01442                                                                   GAS5UPD 
01443 **** IF GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX) > 5                GAS5UPD 
01444      IF GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX) = 5 AND            GAS5UPD 
TB0424*       GAF-INT-TS (GAF-INDEX 5) NOT = HIGH-VALUES                GAS5UPD 
TB0424        GAF-INT-ID (GAF-INDEX 5) NOT = HIGH-VALUES                GAS5UPD 
01446         SET  WT-01-INDEX                     TO +26               GAS5UPD 
01447         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           GAS5UPD 
01448         MOVE SPACES TO  IBGROPTO, IPGPOPTO, IDGDOPTO,             GAS5UPD 
01449                         IPGTOPTO, IPGNOPTO, IPGSOPTO              GAS5UPD 
01450         MOVE SPACES TO  MFRMSLTO                                  GAS5UPD 
01451         MOVE -1 TO PERIODL                                        GAS5UPD 
01452         PERFORM 9010-000-SEND-DATAONLY-RETURN.                    GAS5UPD 
01453                                                                   GAS5UPD 
01454      MOVE GXA-PROVISION-ID          TO     WS-SAVE-INTL-TAB-ID.   GAS5UPD 
01455      MOVE GXA-PROVISION-SLOT-NO     TO     WS-TAB-PROV-COPY-SLOT. GAS5UPD 
01456      MOVE GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  TO                 GAS5UPD 
01457                                            WS-SAVE-INTL-TAB-SLOT. GAS5UPD 
01458                                                                   GAS5UPD 
01459      SET GAF-INT-INDEX  TO  1.                                    GAS5UPD 
01460      SEARCH GAF-INT-TS                                            GAS5UPD 
01461         VARYING GAF-INT-INDEX                                     GAS5UPD 
01462         AT END                                                    GAS5UPD 
01463            MOVE WS-ABCODE-1PL3      TO  WS-ABCODE                 GAS5UPD 
01464            MOVE WS-ABCODE-1PL3-MSG  TO  WS-ABCODE-MSG             GAS5UPD 
01465            PERFORM 9800-000-ERROR-MSG-THEN-ABEND                  GAS5UPD 
01466         WHEN                                                      GAS5UPD 
01467            GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  NOT <             GAS5UPD 
01468                                                 WS-SAVE-INTL-TAB  GAS5UPD 
01469            NEXT SENTENCE.                                         GAS5UPD 
01470                                                                   GAS5UPD 
01471      IF GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  NOT =                GAS5UPD 
01472                                                  WS-SAVE-INTL-TAB GAS5UPD 
01473         PERFORM 2200-225-SHIFT-OCCURS-UP                          GAS5UPD 
01474            VARYING GAF-INT-INDEX  FROM  GAF-INT-INDEX  BY  1      GAS5UPD 
01475            UNTIL GAF-INT-INDEX  >  5                              GAS5UPD 
01476 ******* ADD  1  TO  GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)        GAS5UPD 
01477         IF GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX) < 5             GAS5UPD 
01478            ADD  1  TO  GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)     GAS5UPD 
01479         END-IF                                                    GAS5UPD 
01480      ELSE                                                         GAS5UPD 
01481         MOVE WS-SAVE-INTL-TAB  TO                                 GAS5UPD 
01482                               GAF-INT-TS(GAF-INDEX GAF-INT-INDEX).GAS5UPD 
01483                                                                   GAS5UPD 
01484      GO TO 2200-240-SETUP-GCIO-PARMS.                             GAS5UPD 
01485                                                                   GAS5UPD 
01486                                                                   GAS5UPD 
01487  2200-225-SHIFT-OCCURS-UP.                                        GAS5UPD 
01488      MOVE GAF-INT-TS(GAF-INDEX GAF-INT-INDEX)  TO  WS-INTL-TAB-ID.GAS5UPD 
01489      MOVE WS-SAVE-INTL-TAB  TO                                    GAS5UPD 
01490                             GAF-INT-TS(GAF-INDEX GAF-INT-INDEX).  GAS5UPD 
01491                                                                   GAS5UPD 
01492      MOVE WS-INTL-TAB-ID  TO  WS-SAVE-INTL-TAB.                   GAS5UPD 
01493                                                                   GAS5UPD 
01494                                                                   GAS5UPD 
01495  2200-230-CHANGE-PROD-SLOT-NO.                                    GAS5UPD 
01496                                                                   GAS5UPD 
01497      MOVE GXA-PROVISION-SLOT-NO  TO  WS-TAB-PROV-COPY-SLOT.       GAS5UPD 
01498      SET  GAF-INT-INDEX TO      1.                                GAS5UPD 
01499      SET  GAF-INT-INDEX DOWN BY 1.                                GAS5UPD 
01500                                                                   GAS5UPD 
01501  2200-240-CHANGE-LOOP.                                            GAS5UPD 
01502                                                                   GAS5UPD 
01503      SET GAF-INT-INDEX UP BY 1.                                   GAS5UPD 
01504      IF  GAF-INT-INDEX > 5                                        GAS5UPD 
01505          GO TO 2200-240-SETUP-GCIO-PARMS.                         GAS5UPD 
01506                                                                   GAS5UPD 
01507      IF GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  =  GXA-PROVISION-ID  GAS5UPD 
01508      THEN                                                         GAS5UPD 
01509          MOVE GAF-OCCURS-ENTRY-COUNTER (GAF-INDEX)                GAS5UPD 
01510            TO GXA-PROVISION-SLOT-NO                               GAS5UPD 
01511               GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)              GAS5UPD 
01512          GO TO 2200-240-SETUP-GCIO-PARMS                          GAS5UPD 
01513      ELSE                                                         GAS5UPD 
01514          GO TO 2200-240-CHANGE-LOOP.                              GAS5UPD 
01515                                                                   GAS5UPD 
01516                                                                   GAS5UPD 
01517  2200-240-SETUP-GCIO-PARMS.                                       GAS5UPD 
01518                                                                   GAS5UPD 
01519      IF FRMNUIDI  =  'GS3A'                                       GAS5UPD 
01520         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS5UPD 
01521      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS5UPD 
01522         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS5UPD 
01523      IF FRMNUIDI  =  'GC8A'                                       GAS5UPD 
01524         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS5UPD 
01525         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS5UPD 
01526         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS5UPD 
01527                                                                   GAS5UPD 
01528      MOVE GC-GCPSWORK-DDNAME   TO  GCIO2-FILE-DDNAME.             GAS5UPD 
01529      MOVE GC-GCIO-AREA-1       TO  GCIO2-IO-AREA-TO-USE.          GAS5UPD 
01530      MOVE GXA-PROVISION-ID     TO  GCIO-WRK-TAB-PROVISION-ID.     GAS5UPD 
01531      MOVE GAF-OCCURS-ENTRY-COUNTER (GAF-INDEX)                    GAS5UPD 
01532                                  TO  GCIO-WRK-TAB-PROV-SLOT-NO    GAS5UPD 
01533                                      GXA-PROVISION-SLOT-NO.       GAS5UPD 
01534      MOVE GCIO-WORKFILE-KEY    TO  GCIO2-FILE-KEY                 GAS5UPD 
01535                                    WORK-RECORD-2.                 GAS5UPD 
01536      MOVE WS-TAB-PROV-COPY-SLOT  TO  WRK2-PROV-POOL-COPY-SLOT.    GAS5UPD 
01537                                                                   GAS5UPD 
01538      IF FRMNUIDI  =  'GC8A'                                       GAS5UPD 
01539         MOVE GCA-BEN-PROV-ID  TO  WRK2-ALL-LEV-BEN-PROV.          GAS5UPD 
01540                                                                   GAS5UPD 
01541      MOVE GC-GCIO-ACCESS-CODE-WR  TO  GCIO2-FILE-ACCESS-CODE.     GAS5UPD 
01542                                                                   GAS5UPD 
01543      COMPUTE WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                    GAS5UPD 
01544               GC-GCIOPARM-LEN  +   GCIO2-RECORD-LENGTH.           GAS5UPD 
01545                                                                   GAS5UPD 
01546                                                                   GAS5UPD 
01547  2200-250-UPDATE-ALL-LVL-TAB.                                     GAS5UPD 
01548                                                                   GAS5UPD 
01549 *******                                                           GAS5UPD 
01550 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS5UPD 
01551 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLE UPDATES THEGAS5UPD 
01552 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS5UPD 
01553 *******                                                           GAS5UPD 
01554                                                                   GAS5UPD 
01555      IF  IBGROPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS5UPD 
01556          MOVE MFRMSLTI     TO  IBGRSLTI,   ACWA-DISPLAY-LEN-7     GAS5UPD 
01557          SET  WT-01-INDEX  TO  +01                                GAS5UPD 
01558          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS5UPD 
01559          MOVE -1           TO  IBGROPTL                           GAS5UPD 
01560          MOVE DFHBMABF     TO  IBGRSLTA,   IBGRIDA                GAS5UPD 
01561          MOVE SPACES       TO  IBGROPTI                           GAS5UPD 
01562          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS5UPD 
01563          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS5UPD 
01564          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS5UPD 
01565      IF  IDGDOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS5UPD 
01566          MOVE MFRMSLTI     TO  IDGDSLTI,   ACWA-DISPLAY-LEN-7     GAS5UPD 
01567          SET  WT-01-INDEX  TO  +23                                GAS5UPD 
01568          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS5UPD 
01569          MOVE -1           TO  IDGDOPTL                           GAS5UPD 
01570          MOVE DFHBMABF     TO  IDGDSLTA,   IDGDIDA                GAS5UPD 
01571          MOVE SPACES       TO  IDGDOPTI                           GAS5UPD 
01572          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS5UPD 
01573          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS5UPD 
01574          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS5UPD 
01575      IF  IPGNOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS5UPD 
01576          MOVE MFRMSLTI     TO  IPGNSLTI,   ACWA-DISPLAY-LEN-7     GAS5UPD 
01577          SET  WT-01-INDEX  TO  +02                                GAS5UPD 
01578          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS5UPD 
01579          MOVE -1           TO  IPGNOPTL                           GAS5UPD 
01580          MOVE DFHBMABF     TO  IPGNSLTA,   IPGNIDA                GAS5UPD 
01581          MOVE SPACES       TO  IPGNOPTI                           GAS5UPD 
01582          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS5UPD 
01583          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS5UPD 
01584          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS5UPD 
01585      IF  IPGPOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS5UPD 
01586          MOVE MFRMSLTI     TO  IPGPSLTI,   ACWA-DISPLAY-LEN-7     GAS5UPD 
01587          SET  WT-01-INDEX  TO  +24                                GAS5UPD 
01588          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS5UPD 
01589          MOVE -1           TO  IPGPOPTL                           GAS5UPD 
01590          MOVE DFHBMABF     TO  IPGPSLTA,   IPGPIDA                GAS5UPD 
01591          MOVE SPACES       TO  IPGPOPTI                           GAS5UPD 
01592          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS5UPD 
01593          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS5UPD 
01594          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS5UPD 
01595      IF  IPGSOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS5UPD 
01596          MOVE MFRMSLTI     TO  IPGSSLTI,   ACWA-DISPLAY-LEN-7     GAS5UPD 
01597          SET  WT-01-INDEX  TO  +03                                GAS5UPD 
01598          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS5UPD 
01599          MOVE -1           TO  IPGSOPTL                           GAS5UPD 
01600          MOVE DFHBMABF     TO IPGSSLTA,    IPGSIDA                GAS5UPD 
01601          MOVE SPACES       TO IPGSOPTI                            GAS5UPD 
01602          MOVE DFHBMUNP     TO MFRMSLTA                            GAS5UPD 
01603          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS5UPD 
01604          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS5UPD 
01605      IF  IPGTOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS5UPD 
01606          MOVE MFRMSLTI     TO  IPGTSLTI,   ACWA-DISPLAY-LEN-7     GAS5UPD 
01607          SET  WT-01-INDEX  TO  +03                                GAS5UPD 
01608          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS5UPD 
01609          MOVE -1           TO  IPGTOPTL                           GAS5UPD 
01610          MOVE DFHBMABF     TO IPGTSLTA,    IPGTIDA                GAS5UPD 
01611          MOVE SPACES       TO IPGTOPTI                            GAS5UPD 
01612          MOVE DFHBMUNP     TO MFRMSLTA                            GAS5UPD 
01613          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS5UPD 
01614          GO TO 2200-252-REPLACE-INT-TAB-SLOT                      GAS5UPD 
01615      ELSE                                                         GAS5UPD 
01616          GO TO 2200-253-BYPASS-INT-TAB-SLOT.                      GAS5UPD 
01617                                                                   GAS5UPD 
01618                                                                   GAS5UPD 
01619  2200-252-REPLACE-INT-TAB-SLOT.                                   GAS5UPD 
01620      SET  GAF-INT-INDEX  TO       1.                              GAS5UPD 
01621      SET  GAF-INT-INDEX  DOWN BY  1.                              GAS5UPD 
01622                                                                   GAS5UPD 
01623  2200-252-REPLACE-LOOP.                                           GAS5UPD 
01624                                                                   GAS5UPD 
01625      SET GAF-INT-INDEX  UP BY  1.                                 GAS5UPD 
01626                                                                   GAS5UPD 
01627      IF  GAF-INT-INDEX  >  5                                      GAS5UPD 
01628          MOVE WS-ABCODE-1PLX       TO  WS-ABCODE                  GAS5UPD 
01629          MOVE WS-ABCODE-1PLX-MSG   TO  WS-ABCODE-MSG              GAS5UPD 
01630          PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   GAS5UPD 
01631                                                                   GAS5UPD 
01632      IF  GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  =  GXA-PROVISION-ID GAS5UPD 
01633          MOVE ACWA-DISPLAY-LEN-7  TO                              GAS5UPD 
01634                             GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX) GAS5UPD 
01635          GO TO 2200-252-REPLACE-LOOP-END                          GAS5UPD 
01636      ELSE                                                         GAS5UPD 
01637          GO TO 2200-252-REPLACE-LOOP.                             GAS5UPD 
01638                                                                   GAS5UPD 
01639  2200-252-REPLACE-LOOP-END.                                       GAS5UPD 
01640                                                                   GAS5UPD 
01641  2200-253-BYPASS-INT-TAB-SLOT.                                    GAS5UPD 
01642 *******                                                          |GAS5UPD 
01643 * STS *----------------------------------------------------------*GAS5UPD 
01644 *******                                                           GAS5UPD 
01645                                                                   GAS5UPD 
01646                                                                   GAS5UPD 
01647      PERFORM 3000-000-UPDATE-GAF-RECORD.                          GAS5UPD 
01648                                                                   GAS5UPD 
01649 *******                                                           GAS5UPD 
01650 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS5UPD 
01651 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS5UPD 
01652 *******     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.| GAS5UPD 
01653                                                                   GAS5UPD 
01654      IF  DELADDI   =  'CHG/ADD'  AND  FRMNUIDI  =  'GTM1'  AND    GAS5UPD 
01655          OENTCTRI  NOT =  '0000000' AND                           GAS5UPD 
01656         (IBGROPTI  =  'MT'  OR   IPGNOPTI  =  'MT' OR             GAS5UPD 
01657          IDGDOPTI  =  'MT'  OR   IPGPOPTI  =  'MT' OR             GAS5UPD 
01658          IPGTOPTI  =  'MT'  OR   IPGSOPTI  =  'MT')               GAS5UPD 
01659      THEN                                                         GAS5UPD 
01660          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS5UPD 
01661          MOVE 'CHG/ADD'   TO  DELADDO                             GAS5UPD 
01662          MOVE SPACES      TO  COCURANO                            GAS5UPD 
01663          MOVE DFHBMASD    TO  DLOPTLTA,   DELOPTNA                GAS5UPD 
01664          PERFORM 4100-000-DISPLAY-SKELETON                        GAS5UPD 
01665      ELSE                                                         GAS5UPD 
01666          NEXT SENTENCE.                                           GAS5UPD 
01667 *******                                                         | GAS5UPD 
01668 * STS *---------------------------------------------------------* GAS5UPD 
01669 *******                                                           GAS5UPD 
01670                                                                   GAS5UPD 
01671      IF (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND            GAS5UPD 
01672         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND            GAS5UPD 
01673         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND            GAS5UPD 
01674         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND            GAS5UPD 
01675         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND            GAS5UPD 
01676         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)                GAS5UPD 
01677         GO TO 2200-800.                                           GAS5UPD 
01678                                                                   GAS5UPD 
01679 *******                                                           GAS5UPD 
01680 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS5UPD 
01681 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS5UPD 
01682 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS5UPD 
01683 *******                                                           GAS5UPD 
01684                                                                   GAS5UPD 
01685      IF  DELADDI   =  'CHG/DEL'  AND  FRMNUIDI  =  'GTM1'  AND    GAS5UPD 
01686         (IBGROPTI  =  'MT'  OR  IPGNOPTI  =  'MT'  OR             GAS5UPD 
01687          IDGDOPTI  =  'MT'  OR  IPGPOPTI  =  'MT'  OR             GAS5UPD 
01688          IPGTOPTI  =  'MT'  OR  IPGSOPTI  =  'MT')                GAS5UPD 
01689      THEN                                                         GAS5UPD 
01690          GO TO 2200-800                                           GAS5UPD 
01691      ELSE                                                         GAS5UPD 
01692          NEXT SENTENCE.                                           GAS5UPD 
01693                                                                   GAS5UPD 
01694 *******                                                           GAS5UPD 
01695 * STS *==> DELETES DURING SINGLE TABULAR SUPPORT, THERE IS NEVER  GAS5UPD 
01696 *     *     A WORKFILE INTERNAL TABULAR TO DELETE                 GAS5UPD 
01697 *******                                                           GAS5UPD 
01698                                                                   GAS5UPD 
01699      IF  IBGROPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS5UPD 
01700          MOVE SPACE      TO  IBGROPTO                             GAS5UPD 
01701          MOVE '0000000'  TO  IBGRSLTO                             GAS5UPD 
01702          GO TO 2200-800.                                          GAS5UPD 
01703      IF  IDGDOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS5UPD 
01704          MOVE SPACE      TO  IDGDOPTO                             GAS5UPD 
01705          MOVE '0000000'  TO  IDGDSLTO                             GAS5UPD 
01706          GO TO 2200-800.                                          GAS5UPD 
01707      IF  IPGNOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS5UPD 
01708          MOVE SPACE      TO  IPGNOPTO                             GAS5UPD 
01709          MOVE '0000000'  TO  IPGNSLTO                             GAS5UPD 
01710          GO TO 2200-800.                                          GAS5UPD 
01711      IF  IPGPOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS5UPD 
01712          MOVE SPACE      TO  IPGPOPTO                             GAS5UPD 
01713          MOVE '0000000'  TO  IPGPSLTO                             GAS5UPD 
01714          GO TO 2200-800.                                          GAS5UPD 
01715      IF  IPGTOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS5UPD 
01716          MOVE SPACE      TO  IPGTOPTO                             GAS5UPD 
01717          MOVE '0000000'  TO  IPGTSLTO                             GAS5UPD 
01718          GO TO 2200-800.                                          GAS5UPD 
01719      IF  IPGSOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS5UPD 
01720          MOVE SPACE      TO  IPGSOPTO                             GAS5UPD 
01721          MOVE '0000000'  TO  IPGSSLTO                             GAS5UPD 
01722          GO TO 2200-800.                                          GAS5UPD 
01723                                                                   GAS5UPD 
01724 *******                                                          |GAS5UPD 
01725 * STS *----------------------------------------------------------*GAS5UPD 
01726 *******                                                           GAS5UPD 
01727                                                                   GAS5UPD 
01728      IF IBGROPTI  =  'D' AND  IBGRSLTI  <  '9000000'              GAS5UPD 
01729         MOVE SPACE      TO  IBGROPTO                              GAS5UPD 
01730         MOVE '0000000'  TO  IBGRSLTO                              GAS5UPD 
01731         GO TO 2200-800.                                           GAS5UPD 
01732      IF IDGDOPTI  =  'D' AND  IDGDSLTI  <  '9000000'              GAS5UPD 
01733         MOVE SPACE      TO  IDGDOPTO                              GAS5UPD 
01734         MOVE '0000000'  TO  IDGDSLTO                              GAS5UPD 
01735         GO TO 2200-800.                                           GAS5UPD 
01736      IF IPGNOPTI  =  'D' AND  IPGNSLTI  <  '9000000'              GAS5UPD 
01737         MOVE SPACE      TO  IPGNOPTO                              GAS5UPD 
01738         MOVE '0000000'  TO  IPGNSLTO                              GAS5UPD 
01739         GO TO 2200-800.                                           GAS5UPD 
01740      IF IPGPOPTI  =  'D' AND  IPGPSLTI  <  '9000000'              GAS5UPD 
01741         MOVE SPACE      TO  IPGPOPTO                              GAS5UPD 
01742         MOVE '0000000'  TO  IPGPSLTO                              GAS5UPD 
01743         GO TO 2200-800.                                           GAS5UPD 
01744      IF IPGTOPTI  =  'D' AND  IPGTSLTI  <  '9000000'              GAS5UPD 
01745         MOVE SPACE      TO  IPGTOPTO                              GAS5UPD 
01746         MOVE '0000000'  TO  IPGTSLTO                              GAS5UPD 
01747         GO TO 2200-800.                                           GAS5UPD 
01748      IF IPGSOPTI  =  'D' AND  IPGSSLTI  <  '9000000'              GAS5UPD 
01749         MOVE SPACE      TO  IPGSOPTO                              GAS5UPD 
01750         MOVE '0000000'  TO  IPGSSLTO                              GAS5UPD 
01751         GO TO 2200-800.                                           GAS5UPD 
01752                                                                   GAS5UPD 
01753      IF IBGROPTI  =  'D'                                          GAS5UPD 
01754         MOVE '0000000'  TO  IBGRSLTO                              GAS5UPD 
01755         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS5UPD 
01756      IF IDGDOPTI  =  'D'                                          GAS5UPD 
01757         MOVE '0000000'  TO  IDGDSLTO                              GAS5UPD 
01758         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS5UPD 
01759      IF IPGNOPTI  =  'D'                                          GAS5UPD 
01760         MOVE '0000000'  TO  IPGNSLTO                              GAS5UPD 
01761         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS5UPD 
01762      IF IPGPOPTI  =  'D'                                          GAS5UPD 
01763         MOVE '0000000'  TO  IPGPSLTO                              GAS5UPD 
01764         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS5UPD 
01765      IF IPGTOPTI  =  'D'                                          GAS5UPD 
01766         MOVE '0000000'  TO  IPGTSLTO                              GAS5UPD 
01767         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS5UPD 
01768      IF IPGSOPTI  =  'D'                                          GAS5UPD 
01769         MOVE '0000000'  TO  IPGSSLTO                              GAS5UPD 
01770         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS5UPD 
01771                                                                   GAS5UPD 
01772                                                                   GAS5UPD 
01773  2200-260-CHANGE-INTERNAL-TAB.                                    GAS5UPD 
01774                                                                   GAS5UPD 
01775      SET GCA-RECORD-POINTER TO                                    GAS5UPD 
01776                     ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    GAS5UPD 
01777                                                                   GAS5UPD 
01778      MOVE GCIO-WRK-PLAN-CODE         TO  GCA-PLAN-CODE.           GAS5UPD 
01779      MOVE GCIO-WRK-GROUP-NUM         TO  GCA-GROUP-NUM.           GAS5UPD 
01780      MOVE GCIO-WRK-SECTION-NUM       TO  GCA-SECTION-NUM.         GAS5UPD 
01781      MOVE GCIO-WRK-PKG-CODE          TO  GCA-PKG-CODE.            GAS5UPD 
01782      MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B.               GAS5UPD 
01783      MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL.            GAS5UPD 
01784      MOVE GCIO-WRK-FAMILY-RELATION-LVL  TO                        GAS5UPD 
01785                                          GCA-FAM-REL-LVL.         GAS5UPD 
01786      MOVE GCIO-WRK-EFFDT-CEN         TO  GCA-EFFDT-CEN.           GAS5UPD 
01787      MOVE TABIDI                     TO  GCA-ALL-LEVEL-TAB-ID.    GAS5UPD 
01788      MOVE TABSLTNI                   TO  ACWA-DISPLAY-LEN-7.      GAS5UPD 
01789      MOVE ACWA-DISPLAY-LEN-7         TO  GCA-ALL-LEVEL-TAB-SLOT.  GAS5UPD 
01790      MOVE FUNCTONI          TO   GCA-ALL-LEVEL-TAB-FUNC-CODE.     GAS5UPD 
01791      MOVE GXA-PROVISION-ID           TO  GCA-INTERNAL-TAB-ID.     GAS5UPD 
01792      MOVE OENTCTRI                   TO  GCA-INTERNAL-TAB-SLOT    GAS5UPD 
01793                                          GCA-OCCURS-ENTRY-COUNTER.GAS5UPD 
01794                                                                   GAS5UPD 
01795      IF  DELADDI  =  'CHG/ADD'                                    GAS5UPD 
01796          MOVE 'A'  TO  GCA-ADD-DEL-IND                            GAS5UPD 
01797      ELSE                                                         GAS5UPD 
01798          MOVE 'D'  TO  GCA-ADD-DEL-IND.                           GAS5UPD 
01799                                                                   GAS5UPD 
01800      MOVE GXA-INCLUDE-EXCLUDE-IND  TO  GCA-I-E-INDC.              GAS5UPD 
01801      MOVE FRMNUIDI                 TO  GCA-FROM-MENU-ID.          GAS5UPD 
01802                                                                   GAS5UPD 
01803      IF IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999' OR           GAS5UPD 
01804         IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999' OR           GAS5UPD 
01805         IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999' OR           GAS5UPD 
01806         IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999' OR           GAS5UPD 
01807         IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999' OR           GAS5UPD 
01808         IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'              GAS5UPD 
01809         GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                       GAS5UPD 
01810                                                                   GAS5UPD 
01811      IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            GAS5UPD 
01812         MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                GAS5UPD 
01813         MOVE '1U'      TO  WRK2-CDE-SP                            GAS5UPD 
01814         ADD   1        TO  ACWA-CDE-1U-COUNT                      GAS5UPD 
01815      ELSE                                                         GAS5UPD 
01816         IF CDEINDO   =  ('+CDE+' OR  '+CDE-') AND                 GAS5UPD 
01817            DELADDI   =  'CHG/DEL'                                 GAS5UPD 
01818            IF INTDESKI  =  IDPRODI  AND                           GAS5UPD 
01819               WS-NEW-OCCR-ON-WF  = 'N'         THEN               GAS5UPD 
01820               MOVE '2 '   TO  WRK2-CDE-SP                         GAS5UPD 
01821               ADD   1     TO  ACWA-CDE-2B-COUNT                   GAS5UPD 
01822            ELSE                                                   GAS5UPD 
01823               MOVE '1U'   TO  WRK2-CDE-SP                         GAS5UPD 
01824               ADD   1     TO  ACWA-CDE-1U-COUNT                   GAS5UPD 
01825         ELSE                                                      GAS5UPD 
01826            IF  WS-NEW-OCCR-ON-WF = 'Y'   AND                      GAS5UPD 
01827                CDEINDO  = ('+CDE+'  OR '+CDE-')                   GAS5UPD 
01828                MOVE '1U'     TO WRK2-CDE-SP                       GAS5UPD 
01829                ADD   1       TO ACWA-CDE-1U-COUNT                 GAS5UPD 
01830            ELSE                                                   GAS5UPD 
01831                MOVE '2 '   TO  WRK2-CDE-SP                        GAS5UPD 
01832                ADD   1     TO  ACWA-CDE-2B-COUNT.                 GAS5UPD 
01833                                                                   GAS5UPD 
01834 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS5UPD 
01835      MOVE '1'          TO  GCIO2-OPER-ID-IND.                     GAS5UPD 
01836                                                                   GAS5UPD 
01837      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS5UPD 
01838                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS5UPD 
01839                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS5UPD 
01840                                                                   GAS5UPD 
01841      IF NOT GCIO2-GOOD-RETURN                                     GAS5UPD 
01842         MOVE WS-ABCODE-1PF9       TO  WS-ABCODE                   GAS5UPD 
01843         MOVE WS-ABCODE-1PF9-MSG   TO  WS-ABCODE-MSG               GAS5UPD 
01844         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
01845                                                                   GAS5UPD 
01846      GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                          GAS5UPD 
01847                                                                   GAS5UPD 
01848                                                                   GAS5UPD 
01849  2200-270-DELETE-INTERNAL-TAB.                                    GAS5UPD 
01850                                                                   GAS5UPD 
01851      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS5UPD 
01852         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS5UPD 
01853           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS5UPD 
01854           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS5UPD 
01855                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS5UPD 
01856         EXEC CICS GETMAIN                                         GAS5UPD 
01857                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS5UPD 
01858                INITIMG(WS-HEX-00)                                 GAS5UPD 
01859                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS5UPD 
01860                END-EXEC                                           GAS5UPD 
01861         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS5UPD 
01862                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS5UPD 
01863                                                                   GAS5UPD 
01864      IF FRMNUIDI  =  'GS3A'                                       GAS5UPD 
01865         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS5UPD 
01866      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS5UPD 
01867         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS5UPD 
01868      IF FRMNUIDI  =  'GC8A'                                       GAS5UPD 
01869         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS5UPD 
01870         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS5UPD 
01871         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS5UPD 
01872                                                                   GAS5UPD 
01873      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS5UPD 
01874      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS5UPD 
01875      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS5UPD 
01876      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS5UPD 
01877      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS5UPD 
01878           TO  GXA-ENTRY-COUNT.                                    GAS5UPD 
01879                                                                   GAS5UPD 
01880      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS5UPD 
01881                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS5UPD 
01882                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS5UPD 
01883                                                                   GAS5UPD 
01884      IF NOT GCIO2-GOOD-RETURN                                     GAS5UPD 
01885         MOVE WS-ABCODE-1PF5       TO  WS-ABCODE                   GAS5UPD 
01886         MOVE WS-ABCODE-1PF5-MSG   TO  WS-ABCODE-MSG               GAS5UPD 
01887         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
01888                                                                   GAS5UPD 
01889      MOVE GC-GCIO-ACCESS-CODE-DL TO  GCIO2-FILE-ACCESS-CODE.      GAS5UPD 
01890      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS5UPD 
01891                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS5UPD 
01892                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS5UPD 
01893                                                                   GAS5UPD 
01894      IF NOT GCIO2-GOOD-RETURN                                     GAS5UPD 
01895         MOVE WS-ABCODE-1PFC       TO  WS-ABCODE                   GAS5UPD 
01896         MOVE WS-ABCODE-1PFC-MSG   TO  WS-ABCODE-MSG               GAS5UPD 
01897         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
01898                                                                   GAS5UPD 
01899      IF WRK2-CDE-SP =  '1U'                                       GAS5UPD 
01900         SUBTRACT  1  FROM  ACWA-CDE-1U-COUNT.                     GAS5UPD 
01901                                                                   GAS5UPD 
01902      IF WRK2-CDE-SP  =  '2 '                                      GAS5UPD 
01903         SUBTRACT  1  FROM  ACWA-CDE-2B-COUNT.                     GAS5UPD 
01904                                                                   GAS5UPD 
01905      MOVE SPACES  TO  IBGROPTO,  IPGNOPTO,  IPGTOPTO              GAS5UPD 
01906                       IDGDOPTO,  IPGPOPTO,  IPGSOPTO.             GAS5UPD 
01907      GO TO 2200-800.                                              GAS5UPD 
01908                                                                   GAS5UPD 
01909                                                                   GAS5UPD 
01910  2200-280-XCTL-TO-INT-TAB-PGM.                                    GAS5UPD 
01911                                                                   GAS5UPD 
01912      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS5UPD 
01913         DELADDI  =  'CHG/DEL' AND                                 GAS5UPD 
01914         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS5UPD 
01915         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS5UPD 
01916         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS5UPD 
01917                                                                   GAS5UPD 
01918      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS5UPD 
01919         ACWA-CDE-2B-COUNT  =  ZERO                                GAS5UPD 
01920         NEXT SENTENCE                                             GAS5UPD 
01921      ELSE                                                         GAS5UPD 
01922         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS5UPD 
01923                                                                   GAS5UPD 
01924      MOVE INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.       GAS5UPD 
01925      MOVE 'GAS5UPD' TO DELADD-OPTION.                             GAS5UPD 
01926                                                                   GAS5UPD 
01927      EXEC CICS  XCTL  PROGRAM (WS-INTERNAL-TABULAR-PGM-ID)        GAS5UPD 
01928                 COMMAREA(DFHCOMMAREA)                             GAS5UPD 
01929                 LENGTH  (LENGTH OF DFHCOMMAREA)  END-EXEC.        GAS5UPD 
01930                                                                   GAS5UPD 
01931  2200-800.                                                        GAS5UPD 
01932                                                                   GAS5UPD 
01933      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS5UPD 
01934         DELADDI  =  'CHG/DEL' AND                                 GAS5UPD 
01935         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS5UPD 
01936         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS5UPD 
01937         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS5UPD 
01938                                                                   GAS5UPD 
01939      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS5UPD 
01940         ACWA-CDE-2B-COUNT  =  ZERO                                GAS5UPD 
01941         NEXT SENTENCE                                             GAS5UPD 
01942      ELSE                                                         GAS5UPD 
01943         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS5UPD 
01944                                                                   GAS5UPD 
01945      MOVE -1  TO  PERIODL.                                        GAS5UPD 
01946      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS5UPD 
01947                                                                   GAS5UPD 
01948  2200-900-EXIT. EXIT.                                             GAS5UPD 
01949                                                                   GAS5UPD 
01950 /*****************************************************************GAS5UPD 
01951 *           D E L E T E   T H I S   O C C U R A N C E            *GAS5UPD 
01952 *    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *GAS5UPD 
01953 *  SCREEN'S DISPLAY AND REMOVE THAT ENTRY FROM THE ALL LEVEL     *GAS5UPD 
01954 *  TABULAR RECORD, INCLUDED WITH THAT IS CODE TO DELETE ANY      *GAS5UPD 
01955 *  INTERNAL TABULAR ENTRIES THAT MIGHT BE SPECIFIED BY THAT ENTRY*GAS5UPD 
01956 ******************************************************************GAS5UPD 
01957  2400-000-DELETE-THIS-OCCURANCE SECTION.                          GAS5UPD 
01958  2400-010.                                                        GAS5UPD 
01959                                                                   GAS5UPD 
01960      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS5UPD 
01961                                                                   GAS5UPD 
01962      IF NOT GCIO-GOOD-RETURN                                      GAS5UPD 
01963         MOVE WS-ABCODE-1PFB       TO  WS-ABCODE                   GAS5UPD 
01964         MOVE WS-ABCODE-1PFB-MSG   TO  WS-ABCODE-MSG               GAS5UPD 
01965         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
01966                                                                   GAS5UPD 
01967      IF FRMNUIDI  =  'GS3A'                                       GAS5UPD 
01968         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS5UPD 
01969      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS5UPD 
01970         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS5UPD 
01971      IF FRMNUIDI  =  'GC8A'                                       GAS5UPD 
01972         MOVE 'C6'  TO  GCIO-WRK-RECORD-TYPE                       GAS5UPD 
01973         MOVE GCIO-WRK-TABULAR-PROVISION                           GAS5UPD 
01974                    TO  GCIO-WRK-BENEFIT-PROVISION.                GAS5UPD 
01975                                                                   GAS5UPD 
01976      MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   GAS5UPD 
01977      MOVE OENTCTRI         TO  ACWA-DISPLAY-LEN-7.                GAS5UPD 
01978                                                                   GAS5UPD 
01979         EXEC CICS GETMAIN                                         GAS5UPD 
01980                SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            GAS5UPD 
01981                INITIMG(WS-HEX-00)                                 GAS5UPD 
01982                LENGTH(WS-COPY-TABLE-LEN)                          GAS5UPD 
01983                END-EXEC.                                          GAS5UPD 
01984                                                                   GAS5UPD 
01985         SET ACWA-COPY-TAB-PNTR        TO                          GAS5UPD 
01986                  ADDRESS OF COPY-TABULAR-TABLE-AREA.              GAS5UPD 
01987                                                                   GAS5UPD 
01988      SET GAF-INDEX,  COPY-IDX  TO  1.                             GAS5UPD 
01989                                                                   GAS5UPD 
01990                                                                   GAS5UPD 
01991  2400-100-COPY-SAVED-AND-DELETE.                                  GAS5UPD 
01992                                                                   GAS5UPD 
01993      IF  GAF-INDEX  <  GAF-ENTRY-COUNT                            GAS5UPD 
01994          IF  GAF-OCCURS-ENTRY-COUNTER (GAF-INDEX)  NOT =          GAS5UPD 
01995                    ACWA-DISPLAY-LEN-7                             GAS5UPD 
01996              MOVE GAF-ENTRY          (GAF-INDEX)                  GAS5UPD 
01997                TO COPY-TABULAR-TABLE (COPY-IDX)                   GAS5UPD 
01998              SET  GAF-INDEX,  COPY-IDX  UP BY  1                  GAS5UPD 
01999              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS5UPD 
02000          ELSE                                                     GAS5UPD 
02001 *---- WE ARE NOT GOING TO COPY THE ENTRY THAT IS BEING DELETED.   GAS5UPD 
02002 *---- BUT WE SAVE IT SINCE WE HAVE TO DELETE ANY INTERNAL TABULARSGAS5UPD 
02003              MOVE GAF-ENTRY (GAF-INDEX)  TO  WS-ENTRY             GAS5UPD 
02004              SET  COPY-IDX3  TO  GAF-INDEX                        GAS5UPD 
02005              SET  GAF-INDEX  UP BY  1                             GAS5UPD 
02006              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS5UPD 
02007      ELSE                                                         GAS5UPD 
02008          MOVE GAF-ENTRY          (GAF-INDEX)                      GAS5UPD 
02009            TO COPY-TABULAR-TABLE (COPY-IDX).                      GAS5UPD 
02010                                                                   GAS5UPD 
02011      SET  GAF-ENTRY-COUNT  TO  COPY-IDX.                          GAS5UPD 
02012      MOVE GAF-ENTRY-COUNT  TO  GAF-ENTRY-COUNT.                   GAS5UPD 
02013      SET COPY-IDX,  GAF-INDEX  TO  1.                             GAS5UPD 
02014                                                                   GAS5UPD 
02015                                                                   GAS5UPD 
02016  2400-200-MOVE-UPDATED-TABLE.                                     GAS5UPD 
02017                                                                   GAS5UPD 
02018      IF GAF-INDEX  NOT >  GAF-ENTRY-COUNT                         GAS5UPD 
02019         MOVE COPY-TABULAR-TABLE (COPY-IDX)                        GAS5UPD 
02020           TO GAF-ENTRY          (GAF-INDEX)                       GAS5UPD 
02021         SET GAF-INDEX,  COPY-IDX  UP BY  1                        GAS5UPD 
02022         GO TO 2400-200-MOVE-UPDATED-TABLE.                        GAS5UPD 
02023                                                                   GAS5UPD 
02024 *======== D1218 06/03/88 NG  ==================================   GAS5UPD 
02025       IF CDEINDO  =  '+CDE+'  OR '+CDE-'                          GAS5UPD 
02026          PERFORM 4600-000-UPDATE-CDE-STATUS.                      GAS5UPD 
02027                                                                   GAS5UPD 
02028 *=============================================================    GAS5UPD 
02029      PERFORM 3000-000-UPDATE-GAF-RECORD.                          GAS5UPD 
02030                                                                   GAS5UPD 
02031      SET GAF-INDEX  TO  GAF-ENTRY-COUNT.                          GAS5UPD 
02032      MOVE WS-ENTRY  TO  GAF-ENTRY (GAF-INDEX).                    GAS5UPD 
02033                                                                   GAS5UPD 
02034      IF GAF-INTERNAL-TABULAR-COUNT (GAF-INDEX)  =  1              GAS5UPD 
02035         GO TO 2400-340-DELETE-LOOP-END.                           GAS5UPD 
02036                                                                   GAS5UPD 
02037                                                                   GAS5UPD 
02038 *---- DELETE INTERNAL TABULARS FROM W/F THAT ARE ATTACHED TO      GAS5UPD 
02039 *       ALL LVL TAB OCCURANCE BEING DELETED                       GAS5UPD 
02040  2400-300-DELETE-INTERNAL-TABS.                                   GAS5UPD 
02041      SET GAF-INT-INDEX  TO       1.                               GAS5UPD 
02042      SET GAF-INT-INDEX  DOWN BY  1.                               GAS5UPD 
02043                                                                   GAS5UPD 
02044  2400-320-DELETE-LOOP.                                            GAS5UPD 
02045                                                                   GAS5UPD 
02046      SET GAF-INT-INDEX UP BY 1.                                   GAS5UPD 
02047      IF  GAF-INT-INDEX >  5                                       GAS5UPD 
02048          GO TO 2400-340-DELETE-LOOP-END.                          GAS5UPD 
02049                                                                   GAS5UPD 
02050      IF GAF-INT-ID (GAF-INDEX GAF-INT-INDEX) = HIGH-VALUES        GAS5UPD 
02051         GO TO 2400-340-DELETE-LOOP-END.                           GAS5UPD 
02052                                                                   GAS5UPD 
02053                                                                   GAS5UPD 
02054 ************   09/14/88  NE                                       GAS5UPD 
02055      IF  GAF-INT-SLOT (GAF-INDEX GAF-INT-INDEX)  >  8999999       GAS5UPD 
02056          NEXT SENTENCE                                            GAS5UPD 
02057      ELSE                                                         GAS5UPD 
02058          GO TO  2400-320-DELETE-LOOP.                             GAS5UPD 
02059                                                                   GAS5UPD 
02060      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS5UPD 
02061         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS5UPD 
02062           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS5UPD 
02063           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS5UPD 
02064                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS5UPD 
02065         EXEC CICS GETMAIN                                         GAS5UPD 
02066                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS5UPD 
02067                INITIMG(WS-HEX-00)                                 GAS5UPD 
02068                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS5UPD 
02069                END-EXEC                                           GAS5UPD 
02070         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS5UPD 
02071                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS5UPD 
02072                                                                   GAS5UPD 
02073      MOVE GAF-INT-TS (GAF-INDEX GAF-INT-INDEX)                    GAS5UPD 
02074                                  TO GCIO-WRK-TABULAR-PROVISION.   GAS5UPD 
02075      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS5UPD 
02076      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS5UPD 
02077      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS5UPD 
02078      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS5UPD 
02079      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS5UPD 
02080           TO  GXA-ENTRY-COUNT.                                    GAS5UPD 
02081                                                                   GAS5UPD 
02082      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS5UPD 
02083                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS5UPD 
02084                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS5UPD 
02085      IF NOT GCIO2-GOOD-RETURN                                     GAS5UPD 
02086         MOVE WS-ABCODE-1PF5       TO  WS-ABCODE                   GAS5UPD 
02087         MOVE WS-ABCODE-1PF5-MSG   TO  WS-ABCODE-MSG               GAS5UPD 
02088         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
02089                                                                   GAS5UPD 
02090                                                                   GAS5UPD 
02091      MOVE GC-GCIO-ACCESS-CODE-DL TO GCIO2-FILE-ACCESS-CODE.       GAS5UPD 
02092      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS5UPD 
02093                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS5UPD 
02094                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS5UPD 
02095                                                                   GAS5UPD 
02096      IF NOT GCIO2-GOOD-RETURN                                     GAS5UPD 
02097         MOVE WS-ABCODE-1PFC       TO  WS-ABCODE                   GAS5UPD 
02098         MOVE WS-ABCODE-1PFC-MSG   TO  WS-ABCODE-MSG               GAS5UPD 
02099         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
02100                                                                   GAS5UPD 
02101      IF WRK2-CDE-SP  =  '1U'                                      GAS5UPD 
02102         SUBTRACT 1  FROM  ACWA-CDE-1U-COUNT.                      GAS5UPD 
02103      IF WRK2-CDE-SP  =  '2 '                                      GAS5UPD 
02104         SUBTRACT 1  FROM  ACWA-CDE-2B-COUNT.                      GAS5UPD 
02105                                                                   GAS5UPD 
02106      GO TO 2400-320-DELETE-LOOP.                                  GAS5UPD 
02107                                                                   GAS5UPD 
02108                                                                   GAS5UPD 
02109  2400-340-DELETE-LOOP-END.                                        GAS5UPD 
02110      PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      GAS5UPD 
02111                                                                   GAS5UPD 
02112  2400-400-DISPLAY-SCREEN.                                         GAS5UPD 
02113                                                                   GAS5UPD 
02114      IF COPY-IDX3  =  GAF-ENTRY-COUNT AND  =  1                   GAS5UPD 
02115         MOVE 'CHG/ADD'     TO  DELADDO                            GAS5UPD 
02116         MOVE SPACES        TO  DELOPTNO,   DELOLITO               GAS5UPD 
02117         MOVE DFHBMASD      TO  DLOPTLTA,   DELOPTNA               GAS5UPD 
02118         SET  WT-01-INDEX   TO  +19                                GAS5UPD 
02119         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO         GAS5UPD 
02120         PERFORM 4100-000-DISPLAY-SKELETON.                        GAS5UPD 
02121                                                                   GAS5UPD 
02122      IF  COPY-IDX3  <  GAF-ENTRY-COUNT                            GAS5UPD 
02123      THEN                                                         GAS5UPD 
02124          SET GAF-INDEX  TO  COPY-IDX3                             GAS5UPD 
02125          MOVE SPACES    TO  ERRMSGO                               GAS5UPD 
02126          PERFORM 4400-000-BUILD-DISPLAY                           GAS5UPD 
02127      ELSE                                                         GAS5UPD 
02128          SET  GAF-INDEX     TO  1                                 GAS5UPD 
02129          SET  WT-01-INDEX   TO  +14                               GAS5UPD 
02130          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS5UPD 
02131          PERFORM 4400-000-BUILD-DISPLAY.                          GAS5UPD 
02132                                                                   GAS5UPD 
02133      MOVE SPACES  TO  DELOPTNO.                                   GAS5UPD 
02134      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS5UPD 
02135                                                                   GAS5UPD 
02136  2400-900-EXIT. EXIT.                                             GAS5UPD 
02137                                                                   GAS5UPD 
02138 /*****************************************************************GAS5UPD 
02139 *     P R O C E S S   V A L   L I M I T                           GAS5UPD 
02140 ******************************************************************GAS5UPD 
02141  2600-000-PROCESS-VAL-LIMIT     SECTION.                          GAS5UPD 
02142  2600-010.                                                        GAS5UPD 
02143                                                                   GAS5UPD 
02144      IF (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'NEG' OR                   GAS5UPD 
02145          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG') OR                 GAS5UPD 
02144         (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'UNL' OR                   GAS5UPD 
02145          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL')                    GAS5UPD 
02146          GO TO 2600-900-EXIT.                                     GAS5UPD 
02147                                                                   GAS5UPD 
02148      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS5UPD 
02149      THEN                                                         GAS5UPD 
02150          IF  BENVLQLI  =  '5'                                     GAS5UPD 
02151          THEN                                                     GAS5UPD 
02152              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-7         GAS5UPD 
02153              MOVE ZEROS            TO  ACWA-VALUE-LIMIT-2         GAS5UPD 
02154              GO TO 2600-900-EXIT                                  GAS5UPD 
02155          ELSE                                                     GAS5UPD 
02156              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-9-9       GAS5UPD 
02157              GO TO 2600-900-EXIT                                  GAS5UPD 
02158      ELSE                                                         GAS5UPD 
02159          NEXT SENTENCE.                                           GAS5UPD 
02160                                                                   GAS5UPD 
02161      IF  ACWA-VAL-LIM-SCREEN-1  =  '.'                            GAS5UPD 
02162          MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7       GAS5UPD 
02163          MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2       GAS5UPD 
02164          GO TO 2600-900-EXIT.                                     GAS5UPD 
02165                                                                   GAS5UPD 
02166  2600-900-EXIT. EXIT.                                             GAS5UPD 
02167                                                                   GAS5UPD 
02168 /*****************************************************************GAS5UPD 
02169 * 3000 UPDATE GAD RECORD                                         *GAS5UPD 
02170 *                                                                *GAS5UPD 
02171 *    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *GAS5UPD 
02172 ******************************************************************GAS5UPD 
02173  3000-000-UPDATE-GAF-RECORD     SECTION.                          GAS5UPD 
02174  3000-010.                                                        GAS5UPD 
02175                                                                   GAS5UPD 
02176      COMPUTE  GCIO-RECORD-LENGTH   =                              GAS5UPD 
02177          GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACP-FIXED-LEN  +     GAS5UPD 
02178          (GC-GCTABULR-ACP-VARY-LEN * GAF-ENTRY-COUNT).            GAS5UPD 
02179                                                                   GAS5UPD 
02180 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS5UPD 
02181      MOVE '1'                      TO  GCIO-OPER-ID-IND.          GAS5UPD 
02182      MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     GAS5UPD 
02183                                                                   GAS5UPD 
02184      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS5UPD 
02185                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS5UPD 
02186                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS5UPD 
02187                                                                   GAS5UPD 
02188      IF NOT GCIO-GOOD-RETURN                                      GAS5UPD 
02189         MOVE WS-ABCODE-1PF4       TO  WS-ABCODE                   GAS5UPD 
02190         MOVE WS-ABCODE-1PF4-MSG   TO  WS-ABCODE-MSG               GAS5UPD 
02191         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
02192                                                                   GAS5UPD 
02193  3000-900-EXIT. EXIT.                                             GAS5UPD 
02194                                                                   GAS5UPD 
02195 ******************************************************************GAS5UPD 
02196 * 3100  UNLOCK THE GAB ACCUM TAB RECORD READ EARLIER FOR UPDATE   GAS5UPD 
02197 ******************************************************************GAS5UPD 
02198  3100-RLSE-RU-GAF-REC SECTION.                                    GAS5UPD 
02199                                                                   GAS5UPD 
02200      MOVE GC-GCIO-ACCESS-CODE-UNL  TO  GCIO-FILE-ACCESS-CODE.     GAS5UPD 
02201                                                                   GAS5UPD 
02202      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GAS5UPD 
02203                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS5UPD 
02204                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS5UPD 
02205                                                                   GAS5UPD 
02206      IF NOT GCIO-GOOD-RETURN                                      GAS5UPD 
02207         MOVE WS-ABCODE-1PF4        TO  WS-ABCODE                  GAS5UPD 
02208         MOVE WS-ABCODE-1PF4-MSG    TO  WS-ABCODE-MSG              GAS5UPD 
02209         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
02210  3199-EXIT.     EXIT.                                             GAS5UPD 
02211                                                                   GAS5UPD 
02212 /*****************************************************************GAS5UPD 
02213 * 3200  READ REC FOR UPDATE                                      *GAS5UPD 
02214 *                                                                *GAS5UPD 
02215 *    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *GAS5UPD 
02216 *  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *GAS5UPD 
02217 ******************************************************************GAS5UPD 
02218  3200-000-READ-REC-FOR-UPDATE   SECTION.                          GAS5UPD 
02219  3300-010.                                                        GAS5UPD 
02220                                                                   GAS5UPD 
02221      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN   +  GAS5UPD 
02222            GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACP-FIXED-LEN   +  GAS5UPD 
02223            (GC-GCTABULR-ACP-VARY-LEN   *                          GAS5UPD 
02224                                  GC-GCTABULR-ACP-VARY-MAX-OCUR).  GAS5UPD 
02225                                                                   GAS5UPD 
02226      IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS5UPD 
02227         NEXT SENTENCE                                             GAS5UPD 
02228      ELSE                                                         GAS5UPD 
02229         EXEC CICS GETMAIN                                         GAS5UPD 
02230                SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      GAS5UPD 
02231                INITIMG(WS-HEX-00)                                 GAS5UPD 
02232                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS5UPD 
02233                END-EXEC                                           GAS5UPD 
02234         SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     GAS5UPD 
02235                  ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        GAS5UPD 
02236                                                                   GAS5UPD 
02237                                                                   GAS5UPD 
02238      IF FRMNUIDI  =  'GS3A'                                       GAS5UPD 
02239         PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    GAS5UPD 
02240      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS5UPD 
02241         PERFORM 6100-000-BUILD-CONTRACT-KEY.                      GAS5UPD 
02242      IF FRMNUIDI  =  'GC8A'                                       GAS5UPD 
02243         PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      GAS5UPD 
02244                                                                   GAS5UPD 
02245      MOVE GC-GCPSWORK-DDNAME      TO  GCIO-FILE-DDNAME.           GAS5UPD 
02246      MOVE GC-GCIO-AREA-1          TO  GCIO-IO-AREA-TO-USE.        GAS5UPD 
02247      MOVE GCIO-WORKFILE-KEY       TO  GCIO-FILE-KEY.              GAS5UPD 
02248      MOVE GC-GCIO-ACCESS-CODE-RU  TO  GCIO-FILE-ACCESS-CODE.      GAS5UPD 
02249                                                                   GAS5UPD 
02250      MOVE GC-GCTABULR-ACP-VARY-MAX-OCUR  TO  GAF-ENTRY-COUNT.     GAS5UPD 
02251                                                                   GAS5UPD 
02252      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS5UPD 
02253                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS5UPD 
02254                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS5UPD 
02255                                                                   GAS5UPD 
02256  3200-900-EXIT. EXIT.                                             GAS5UPD 
02257                                                                   GAS5UPD 
02258 /*****************************************************************GAS5UPD 
02259 * 4100  DISPLAY SKELETON                                         *GAS5UPD 
02260 *                                                                *GAS5UPD 
02261 *    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *GAS5UPD 
02262 *  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *GAS5UPD 
02263 *  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *GAS5UPD 
02264 ******************************************************************GAS5UPD 
02265  4100-000-DISPLAY-SKELETON      SECTION.                          GAS5UPD 
02266  4100-010.                                                        GAS5UPD 
02267                                                                   GAS5UPD 
02268      MOVE SPACES    TO  ERRMSGO.                                  GAS5UPD 
02269                                                                   GAS5UPD 
02270      MOVE DFHBMFSE  TO  PERIODA.                                  GAS5UPD 
02271                                                                   GAS5UPD 
02272      MOVE DFHBMUNP  TO  BENVLQLA FAMINDIA  INTDESKA  LOBA         GAS5UPD 
02273                         IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA     GAS5UPD 
02274                         IDGDOPTA IPGPOPTA  IPGSOPTA.              GAS5UPD 
02275                                                                   GAS5UPD 
02276      MOVE ALL '_'  TO  PERIODO   BENVLQLO  LOBO                   GAS5UPD 
02277                        FAMINDIO  PLCTRMTO.                        GAS5UPD 
02278                                                                   GAS5UPD 
02279      MOVE LOW-VALUES  TO  INTDESKO  MFRMSLTO  IDGDOPTO IPGPOPTO   GAS5UPD 
02280                           IPGTOPTO  IBGROPTO  IPGNOPTO IPGSOPTO.  GAS5UPD 
02281                                                                   GAS5UPD 
02282      MOVE ZEROS  TO  COPAYINO  CSTCONTO  PERTQALO  BISNDINO       GAS5UPD 
02283            DAYFACIO  SRVGRUPO  PRTIMEFO  MANAPLIO  TIMEDOLO       GAS5UPD 
02284                      CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO       GAS5UPD 
02285            FYIVALO   OVRDINDO  NEWVALUO  DEFINTNO                 GAS5UPD 
02286            CONDALLO  CONDEXCO  CONDICDO  CONDTABO  CONDMENO       GAS5UPD 
02287            CONDEMCO  CONDEACO  CONDSMIO  CONDNSMO                 GAS5UPD 
02288         CONDDRGO  CONDALCO  CONDOBNO  CONDOBCO  CONDMALO CONDTMJO GAS5UPD 
02289         CONDCARO  CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO CONDINFO GAS5UPD 
02290            PRTIMEFO  INTRVALO  CONDPECO  CONDNEMO  NEWVALUO       GAS5UPD 
02291            OENTCTRO  IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO       GAS5UPD 
02292            AGEQLLO   AGEQLHO   CONDLIFO  IPGSSLTO                 GAS5UPD 
02293            RELPINDO  AGELIMLO  AGELIMHO IDGDSLTO  IPGPSLTO        GAS5UPD 
02294            FEAKINDO  ACCUMIDO  CAPINDO  SABDINDO                  GAS5UPD 
02294            BENTYPO   TIERCDO   TIERLVO.                           GAS5UPD 
02295                                                                   GAS5UPD 
02296      MOVE '01'    TO  COCURANO.                                   GAS5UPD 
02297      MOVE -1      TO  PERIODL.                                    GAS5UPD 
02298                                                                   GAS5UPD 
02299      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS5UPD 
02300                                                                   GAS5UPD 
02301  4100-900-EXIT. EXIT.                                             GAS5UPD 
02302                                                                   GAS5UPD 
02303 /*****************************************************************GAS5UPD 
02304 * 4400 BUILD DISPLAY                                             *GAS5UPD 
02305 *                                                                *GAS5UPD 
02306 *    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *GAS5UPD 
02307 *  SPECIFIED BY INDEX GAF-INDEX TO THE SCREEN.                   *GAS5UPD 
02308 ******************************************************************GAS5UPD 
02309  4400-000-BUILD-DISPLAY         SECTION.                          GAS5UPD 
02310  4400-010.                                                        GAS5UPD 
02311                                                                   GAS5UPD 
02312      IF  DELADDI  =  'CHG/DEL'                                    GAS5UPD 
02313      THEN                                                         GAS5UPD 
02314          MOVE 'D'  TO  DELOLITO                                   GAS5UPD 
02315      ELSE                                                         GAS5UPD 
02316          MOVE SPACE  TO  DELOLITO.                                GAS5UPD 
02317                                                                   GAS5UPD 
02318      MOVE SPACE                                       TO DELOPTNO.GAS5UPD 
02319      MOVE GAF-OCCURS-ENTRY-COUNTER     (GAF-INDEX)  TO            GAS5UPD 
02320                                                ACWA-DISPLAY-LEN-7.GAS5UPD 
02321      MOVE ACWA-DISPLAY-LEN-7                        TO  OENTCTRO. GAS5UPD 
02322      MOVE GAF-COPAY-DAY-FACTOR-IND     (GAF-INDEX)  TO  DAYFACIO. GAS5UPD 
02323      MOVE GAF-COPAY-CO-PAY-IND         (GAF-INDEX)  TO  COPAYINO. GAS5UPD 
02324      MOVE GAF-COPAY-DEFINITION         (GAF-INDEX)  TO  DEFINTNO. GAS5UPD 
02325      MOVE GAF-COPAY-MANDATORY-IND      (GAF-INDEX)  TO  MANAPLIO. GAS5UPD 
02326      MOVE GAF-COPAY-TIME-DOLLAR-IND    (GAF-INDEX)  TO  TIMEDOLO. GAS5UPD 
02327      MOVE GAF-COPAY-COST-CONTAIN-IND   (GAF-INDEX)  TO  CSTCONTO. GAS5UPD 
02328      MOVE GAF-COPAY-BENEFIT-PERIOD     (GAF-INDEX)  TO  PERIODO.  GAS5UPD 
02329      MOVE GAF-COPAY-BEN-PER-TIME-QUAL  (GAF-INDEX)  TO  PERTQALO. GAS5UPD 
02330      MOVE GAF-COPAY-FAM-OR-INDIV       (GAF-INDEX)  TO  FAMINDIO. GAS5UPD 
02331      MOVE GAF-COPAY-PLACE-OF-TREATMENT (GAF-INDEX)  TO  PLCTRMTO. GAS5UPD 
02332      MOVE GAF-COPAY-SERVICE-GROUP      (GAF-INDEX)  TO  SRVGRUPO. GAS5UPD 
02333      MOVE GAF-COPAY-RELATIONSHIP-IND   (GAF-INDEX)  TO  RELPINDO. GAS5UPD 
02334      MOVE GAF-COPAY-AGE-QUAL-IND-FROM  (GAF-INDEX)  TO  AGEQLLO.  GAS5UPD 
02335      MOVE GAF-COPAY-AGE-QUAL-IND-TO    (GAF-INDEX)  TO  AGEQLHO.  GAS5UPD 
02336      MOVE GAF-COPAY-AGE-LIMIT-FROM     (GAF-INDEX)  TO            GAS5UPD 
02337                                                ACWA-DISPLAY-LEN-3.GAS5UPD 
02338      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMLO. GAS5UPD 
02339      MOVE GAF-COPAY-AGE-LIMIT-TO       (GAF-INDEX)  TO            GAS5UPD 
02340                                                ACWA-DISPLAY-LEN-3.GAS5UPD 
02341      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMHO. GAS5UPD 
           MOVE GAF-COPAY-BISCENDING-IND     (GAF-INDEX)  TO  BISNDINO.         
           MOVE GAF-COPAY-ASCEND-DESCEND-IND (GAF-INDEX)  TO  ASCDSCDO.         
      **P21595 CHANGES STARTS                                                   
           MOVE GAF-COPAY-BEN-TYPE           (GAF-INDEX)  TO  BENTYPO.          
           MOVE GAF-COPAY-TIER-CODE          (GAF-INDEX)  TO  TIERCDO.          
           MOVE GAF-COPAY-TIER-LVL           (GAF-INDEX)  TO  TIERLVO.          
      **P21595 CHANGES ENDS                                                     
02342      MOVE GAF-COPAY-FEAK-IND           (GAF-INDEX)  TO  FEAKINDO. GAS5UPD 
02343      MOVE GAF-COPAY-ACCUMID            (GAF-INDEX)  TO  ACCUMIDO. GAS5UPD 
02344      MOVE GAF-COPAY-COMB-APPLIED-IND   (GAF-INDEX)  TO  CAPINDO.  GAS5UPD 
02345      MOVE GAF-COPAY-SEL-ADDL-BEN-DET   (GAF-INDEX)  TO  SABDINDO. GAS5UPD 
02346      MOVE GAF-COPAY-BEN-PER-TIME-FCTR  (GAF-INDEX)  TO            GAS5UPD 
02347                                                ACWA-DISPLAY-LEN-3.GAS5UPD 
02348      MOVE ACWA-DISPLAY-LEN-3                        TO  PRTIMEFO. GAS5UPD 
02349      MOVE GAF-COPAY-CLAIM-LVL-ACCUM-IND (GAF-INDEX) TO  CLMLVLIO. GAS5UPD 
02350      MOVE GAF-COPAY-INTERVAL-TIME-FCTR (GAF-INDEX)  TO            GAS5UPD 
02351                                                ACWA-DISPLAY-LEN-3.GAS5UPD 
02352      MOVE ACWA-DISPLAY-LEN-3                        TO  INTRVALO. GAS5UPD 
02353      MOVE GAF-COPAY-INTERVAL-TYPE      (GAF-INDEX)  TO  INTTYPEO. GAS5UPD 
02354      MOVE GAF-COPAY-L-O-B              (GAF-INDEX)  TO  LOBO.     GAS5UPD 
02355      MOVE GAF-COPAY-VALUE-LIMIT        (GAF-INDEX)  TO            GAS5UPD 
02356                                                ACWA-VALUE-LIMIT-9.GAS5UPD 
02357      IF  ACWA-VALUE-LIMIT-9-9 = -1                                GAS5UPD 
02358          MOVE 'NEG' TO BNMXVALO                                   GAS5UPD 
02359      ELSE                                                         GAS5UPD 
02357      IF  ACWA-VALUE-LIMIT-9-9 = -2                                GAS5UPD 
02358          MOVE 'UNL' TO BNMXVALO                                   GAS5UPD 
02359      ELSE                                                         GAS5UPD 
02360          IF  GAF-COPAY-VALUE-QUALIFIER (GAF-INDEX) = '5'          GAS5UPD 
02361              MOVE ACWA-VALUE-LIMIT-9     TO  ACWA-EDIT-VALUE-LIMITGAS5UPD 
02362              MOVE ACWA-EDIT-VALUE-LIMIT  TO  BNMXVALO             GAS5UPD 
02363          ELSE                                                     GAS5UPD 
02364              MOVE GAF-COPAY-VALUE-LIMIT(GAF-INDEX) TO             GAS5UPD 
02365                                               ACWA-DISPLAY-LEN-9-2GAS5UPD 
02366              MOVE ACWA-DISPLAY-LEN-9-X   TO  ACWA-DISPLAY-9       GAS5UPD 
02367              MOVE SPACES                 TO  ACWA-DISPLAY-1       GAS5UPD 
02368              MOVE ACWA-DISPLAY-VALUE-LIMIT  TO  BNMXVALO.         GAS5UPD 
02369                                                                   GAS5UPD 
02370      MOVE GAF-COPAY-VALUE-QUALIFIER   (GAF-INDEX)  TO  BENVLQLO.  GAS5UPD 
02371      MOVE GAF-COPAY-INTERVAL-OVRD-VALUE(GAF-INDEX) TO             GAS5UPD 
02372                                                ACWA-DISPLAY-LEN-5.GAS5UPD 
02373      MOVE ACWA-DISPLAY-LEN-5                       TO  NEWVALUO.  GAS5UPD 
02374      MOVE GAF-COPAY-INTERVAL-OVRD-IND (GAF-INDEX)  TO  OVRDINDO.  GAS5UPD 
02375      MOVE GAF-COPAY-INTERNAL-DESCRIPTOR(GAF-INDEX) TO  INTDESKO.  GAS5UPD 
02376      MOVE GAF-COND-ALL-BIT            (GAF-INDEX)  TO  CONDALLO.  GAS5UPD 
02377      MOVE GAF-COND-EXCLUSION-BIT      (GAF-INDEX)  TO  CONDEXCO.  GAS5UPD 
02378      MOVE GAF-COND-ICD-BIT            (GAF-INDEX)  TO  CONDICDO.  GAS5UPD 
02379      MOVE GAF-COND-TB-BIT             (GAF-INDEX)  TO  CONDTABO.  GAS5UPD 
02380      MOVE GAF-COND-MENTAL-BIT         (GAF-INDEX)  TO  CONDMENO.  GAS5UPD 
02381      MOVE GAF-COND-DRUG-BIT           (GAF-INDEX)  TO  CONDDRGO.  GAS5UPD 
02382      MOVE GAF-COND-ALCOHOL-BIT        (GAF-INDEX)  TO  CONDALCO.  GAS5UPD 
02383      MOVE GAF-COND-OB-COMP-BIT        (GAF-INDEX)  TO  CONDOBCO.  GAS5UPD 
02384      MOVE GAF-COND-OB-NORM-BIT        (GAF-INDEX)  TO  CONDOBNO.  GAS5UPD 
02385      MOVE GAF-COND-MALIGNANCY-BIT     (GAF-INDEX)  TO  CONDMALO.  GAS5UPD 
02386      MOVE GAF-COND-CARDIAC-DISEASE-BIT(GAF-INDEX)  TO  CONDCARO.  GAS5UPD 
02387      MOVE GAF-COND-OBESITY-BIT        (GAF-INDEX)  TO  CONDOBSO.  GAS5UPD 
02388      MOVE GAF-COND-KIDNEY-DISEASE-BIT (GAF-INDEX)  TO  CONDKDYO.  GAS5UPD 
02389      MOVE GAF-COND-ACCIDENT-BIT       (GAF-INDEX)  TO  CONDACCO.  GAS5UPD 
02390      MOVE GAF-COND-PRE-EXIST-BIT      (GAF-INDEX)  TO  CONDPECO.  GAS5UPD 
02391      MOVE GAF-COND-NON-EMER-BIT       (GAF-INDEX)  TO  CONDNEMO.  GAS5UPD 
02392      MOVE GAF-COND-SUICIDE-BIT        (GAF-INDEX)  TO  CONDSUIO.  GAS5UPD 
02393      MOVE GAF-COND-TMJ-BIT            (GAF-INDEX)  TO  CONDTMJO.  GAS5UPD 
02394      MOVE GAF-COND-INF-BIT            (GAF-INDEX)  TO  CONDINFO.  GAS5UPD 
02395      MOVE GAF-COND-EMER-MED-BIT       (GAF-INDEX)  TO  CONDEMCO.  GAS5UPD 
02396      MOVE GAF-COND-EMER-ACC-BIT       (GAF-INDEX)  TO  CONDEACO.  GAS5UPD 
02397      MOVE GAF-COND-SER-MEN-ILL-BIT    (GAF-INDEX)  TO  CONDSMIO.  GAS5UPD 
02398      MOVE GAF-COND-NON-SER-MEN-ILL-BIT (GAF-INDEX)  TO  CONDNSMO. GAS5UPD 
02399                                                                   GAS5UPD 
02400      MOVE -1  TO  PERIODL.                                        GAS5UPD 
02401                                                                   GAS5UPD 
02402      MOVE GAF-COPAY-FYI-VALUE(GAF-INDEX) TO  FYIVALO.             GAS5UPD 
02403      SET  CURNT-OCURS-BIN        TO  GAF-INDEX.                   GAS5UPD 
02404      MOVE CURNT-OCURS-BIN        TO  CURNT-OCURS-PKD.             GAS5UPD 
02405      MOVE CURNT-OCCURS-OUT       TO  COCURANO.                    GAS5UPD 
02406                                                                   GAS5UPD 
02407      IF GAF-ENTRY-COUNT  >  1                                     GAS5UPD 
02408         COMPUTE  TOTAL-OCURS-UNK  =  GAF-ENTRY-COUNT  - 1         GAS5UPD 
02409         MOVE  TOTAL-OCCURS-OUT    TO  TOCURANO                    GAS5UPD 
02410      ELSE                                                         GAS5UPD 
02411         MOVE  '01'                TO  TOCURANO.                   GAS5UPD 
02412                                                                   GAS5UPD 
02413                                                                   GAS5UPD 
02414      MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              GAS5UPD 
02415                       IDGDSLTO,  IPGPSLTO,  IPGSSLTO.             GAS5UPD 
02416                                                                   GAS5UPD 
02417      SET GAF-INT-INDEX  TO       1.                               GAS5UPD 
02418      SET GAF-INT-INDEX  DOWN BY  1.                               GAS5UPD 
02419                                                                   GAS5UPD 
02420  4400-300-DISPLAY-LOOP.                                           GAS5UPD 
02421                                                                   GAS5UPD 
02422      SET GAF-INT-INDEX  UP BY  1.                                 GAS5UPD 
02423      IF  GAF-INT-INDEX  >  5                                      GAS5UPD 
02424          GO TO 4400-800-SEND.                                     GAS5UPD 
02425                                                                   GAS5UPD 
02426      IF  GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  =  HIGH-VALUES      GAS5UPD 
02427          GO TO 4400-800-SEND.                                     GAS5UPD 
02428                                                                   GAS5UPD 
02429      IF  GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)   =   '#IBGR '       GAS5UPD 
02430          MOVE GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)               GAS5UPD 
02431                                   TO  ACWA-DISPLAY-LEN-7          GAS5UPD 
02432          MOVE ACWA-DISPLAY-LEN-7  TO  IBGRSLTO                    GAS5UPD 
02433          GO TO 4400-300-DISPLAY-LOOP.                             GAS5UPD 
02434                                                                   GAS5UPD 
02435      IF  GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)   =   '#IDGD '       GAS5UPD 
02436          MOVE GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)               GAS5UPD 
02437                                   TO  ACWA-DISPLAY-LEN-7          GAS5UPD 
02438          MOVE ACWA-DISPLAY-LEN-7  TO  IDGDSLTO                    GAS5UPD 
02439          GO TO 4400-300-DISPLAY-LOOP.                             GAS5UPD 
02440                                                                   GAS5UPD 
02441      IF  GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)   =   '#IPGN '       GAS5UPD 
02442          MOVE GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)               GAS5UPD 
02443                                   TO  ACWA-DISPLAY-LEN-7          GAS5UPD 
02444          MOVE ACWA-DISPLAY-LEN-7  TO  IPGNSLTO                    GAS5UPD 
02445          GO TO 4400-300-DISPLAY-LOOP.                             GAS5UPD 
02446                                                                   GAS5UPD 
02447      IF  GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)   =   '#IPGP '       GAS5UPD 
02448          MOVE GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)               GAS5UPD 
02449                                   TO  ACWA-DISPLAY-LEN-7          GAS5UPD 
02450          MOVE ACWA-DISPLAY-LEN-7  TO  IPGPSLTO                    GAS5UPD 
02451          GO TO 4400-300-DISPLAY-LOOP.                             GAS5UPD 
02452                                                                   GAS5UPD 
02453      IF  GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)   =   '#IPGT '       GAS5UPD 
02454          MOVE GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)               GAS5UPD 
02455                                   TO  ACWA-DISPLAY-LEN-7          GAS5UPD 
02456          MOVE ACWA-DISPLAY-LEN-7  TO  IPGTSLTO                    GAS5UPD 
02457          GO TO 4400-300-DISPLAY-LOOP.                             GAS5UPD 
02458                                                                   GAS5UPD 
02459      IF  GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)   =   '#IPGS '       GAS5UPD 
02460          MOVE GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)               GAS5UPD 
02461                                   TO  ACWA-DISPLAY-LEN-7          GAS5UPD 
02462          MOVE ACWA-DISPLAY-LEN-7  TO  IPGSSLTO                    GAS5UPD 
02463          GO TO 4400-300-DISPLAY-LOOP.                             GAS5UPD 
02464                                                                   GAS5UPD 
02465      MOVE WS-ABCODE-1PF3       TO  WS-ABCODE                      GAS5UPD 
02466      MOVE WS-ABCODE-1PF3-MSG   TO  WS-ABCODE-MSG                  GAS5UPD 
02467      PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       GAS5UPD 
02468                                                                   GAS5UPD 
02469                                                                   GAS5UPD 
02470  4400-800-SEND.                                                   GAS5UPD 
02471                                                                   GAS5UPD 
02472      PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      GAS5UPD 
02473                                                                   GAS5UPD 
02474      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS5UPD 
02475                                                                   GAS5UPD 
02476  4400-900-EXIT. EXIT.                                             GAS5UPD 
02477 /*****************************************************************GAS5UPD 
02478 *  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *GAS5UPD 
02479 *                                                                *GAS5UPD 
02480 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS5UPD 
02481 *          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *GAS5UPD 
02482 *          2. IF GROUP IS CRITICAL:                              *GAS5UPD 
02483 *              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *GAS5UPD 
02484 *                BENEFIT PROVISION.                              *GAS5UPD 
02485 *                - IF ON DATA BASE:                              *GAS5UPD 
02486 *                  - SCAN FOR #ACP TABULAR                       *GAS5UPD 
02487 *                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *GAS5UPD 
02488 *                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *GAS5UPD 
02489 *                      ON SCREEN AND ISSUE MESSAGE.              *GAS5UPD 
02490 ******************************************************************GAS5UPD 
02491  4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          GAS5UPD 
02492  4500-010.                                                        GAS5UPD 
02493                                                                   GAS5UPD 
02494      IF DELADDI  =  'CHG/DEL'     OR                              GAS5UPD 
02495         DELOLITI =  SPACES                                        GAS5UPD 
02496      THEN                                                         GAS5UPD 
02497         NEXT SENTENCE                                             GAS5UPD 
02498      ELSE                                                         GAS5UPD 
02499         GO TO 4500-900-EXIT.                                      GAS5UPD 
02500                                                                   GAS5UPD 
02501                                                                   GAS5UPD 
02502      MOVE WS-REQUEST-4500-CDE-PROTECT  TO  ACWA-CDE-REQUEST-CODE. GAS5UPD 
02503                                                                   GAS5UPD 
02504      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS5UPD 
02505                 COMMAREA (DFHCOMMAREA)                            GAS5UPD 
02506                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS5UPD 
02507                                                                   GAS5UPD 
02508      GO TO 4500-900-EXIT.                                         GAS5UPD 
02509                                                                   GAS5UPD 
02510                                                                   GAS5UPD 
02511  4500-900-EXIT.   EXIT.                                           GAS5UPD 
02512                                                                   GAS5UPD 
02513 /*****************************************************************GAS5UPD 
02514 *  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *GAS5UPD 
02515 *                                                                *GAS5UPD 
02516 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS5UPD 
02517 *           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *GAS5UPD 
02518 *           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *GAS5UPD 
02519 *               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *GAS5UPD 
02520 *           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *GAS5UPD 
02521 *               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *GAS5UPD 
02522 *               +CDE+ INDICATOR (POSITION=8).                    *GAS5UPD 
02523 *              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *GAS5UPD 
02524 *               ENTER, CONTINUE PROCESSING.                      *GAS5UPD 
02525 ******************************************************************GAS5UPD 
02526  4600-000-UPDATE-CDE-STATUS     SECTION.                          GAS5UPD 
02527  4600-010.                                                        GAS5UPD 
02528                                                                   GAS5UPD 
02529                                                                   GAS5UPD 
02530      SET  ACWA-INDEX-1    TO  GAF-INDEX.                          GAS5UPD 
02531      MOVE WS-REQUEST-4600-CDE-STATUS  TO  ACWA-CDE-REQUEST-CODE.  GAS5UPD 
02532                                                                   GAS5UPD 
02533      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS5UPD 
02534                 COMMAREA (DFHCOMMAREA)                            GAS5UPD 
02535                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS5UPD 
02536                                                                   GAS5UPD 
02537      IF  ACWA-CDE-RETURN-DONT-SEND                                GAS5UPD 
02538          EXEC CICS  RETURN   END-EXEC.                            GAS5UPD 
02539                                                                   GAS5UPD 
02540  4600-900-EXIT.   EXIT.                                           GAS5UPD 
02541                                                                   GAS5UPD 
02542 /*****************************************************************GAS5UPD 
02543 *  4700  -  UPDATE W/F CONTROL RECORD                            *GAS5UPD 
02544 *                                                                *GAS5UPD 
02545 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS5UPD 
02546 *          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *GAS5UPD 
02547 *             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *GAS5UPD 
02548 *             RECORD AND THE INTERNAL TABULAR RECORDS; IF THE    *GAS5UPD 
02549 *             CDE STATUS HAS CHANGED.                            *GAS5UPD 
02550 *          2. REWRITE W/F CONTROL RECORD                         *GAS5UPD 
02551 ******************************************************************GAS5UPD 
02552  4700-000-UPDATE-CONTROL-RECORD SECTION.                          GAS5UPD 
02553  4700-010.                                                        GAS5UPD 
02554                                                                   GAS5UPD 
02555      SET  ACWA-INDEX-1     TO  GAF-INDEX.                         GAS5UPD 
02556      MOVE WS-REQUEST-4700-CNTL-UPDATE  TO  ACWA-CDE-REQUEST-CODE. GAS5UPD 
02557                                                                   GAS5UPD 
02558      EXEC CICS  LINK   PROGRAM ('GACDEPGM')                       GAS5UPD 
02559                 COMMAREA (DFHCOMMAREA)                            GAS5UPD 
02560                 LENGTH(LENGTH OF DFHCOMMAREA)      END-EXEC.      GAS5UPD 
02561                                                                   GAS5UPD 
02562  4700-900-EXIT.  EXIT.                                            GAS5UPD 
02563                                                                   GAS5UPD 
02564 ******************************************************************GAS5UPD 
02565 *  4900  -  R E S E T   O T H E R   I N T E R N A L   T A B S     GAS5UPD 
02566 *                                                                 GAS5UPD 
02567 *    FUNCTION  (VIA CDE MODULE GACDEPGM)                          GAS5UPD 
02568 *          READ W/F ACCUM'S INTERNAL TABULAR RECORDS, THOSE ON    GAS5UPD 
02569 *          W/F ONLY.  RESET THE CDE STATUS INDICATOR ON THIS      GAS5UPD 
02570 *          INTERNAL TO EITHER 1U OR 2 BASED ON THE INTERNAL       GAS5UPD 
02571 *          DESCRIPTOR, THEN REWRITE THIS RECORD.                  GAS5UPD 
02572 ******************************************************************GAS5UPD 
02573  4900-RESET-OTHER-INT-TABS      SECTION.                          GAS5UPD 
02574                                                                   GAS5UPD 
02575      MOVE ZERO  TO  WS-INTRNL-TABS-TO-CHG-CNT.                    GAS5UPD 
02576      PERFORM 4900-COUNT-INT-TAB                                   GAS5UPD 
02577         VARYING GAF-INT-INDEX  FROM  1  BY  1                     GAS5UPD 
02578         UNTIL GAF-INT-INDEX  NOT <                                GAS5UPD 
02579                          GAF-INTERNAL-TABULAR-COUNT(GAF-INDEX)  ORGAS5UPD 
02580               GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  =  HIGH-VALUES.GAS5UPD 
02581                                                                   GAS5UPD 
02582      IF WS-INTRNL-TABS-TO-CHG-CNT  >  ZERO                        GAS5UPD 
02583         MOVE WS-REQUEST-4900-CNTL-UPDATE  TO                      GAS5UPD 
02584                                            ACWA-CDE-REQUEST-CODE  GAS5UPD 
02585         EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                   GAS5UPD 
02586                    COMMAREA (DFHCOMMAREA)                         GAS5UPD 
02587                    LENGTH(LENGTH OF DFHCOMMAREA)        END-EXEC. GAS5UPD 
02588                                                                   GAS5UPD 
02589      GO TO 4999-EXIT.                                             GAS5UPD 
02590                                                                   GAS5UPD 
02591  4900-COUNT-INT-TAB.                                              GAS5UPD 
02592      IF GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)  >  +8999999        GAS5UPD 
02593         ADD 1  TO  WS-INTRNL-TABS-TO-CHG-CNT.                     GAS5UPD 
02594                                                                   GAS5UPD 
02595  4999-EXIT.       EXIT.                                           GAS5UPD 
02596 /*****************************************************************GAS5UPD 
02597 *     READ ALL LEVEL TABULAR FROM PROVISION POOL                  GAS5UPD 
02598 *                                                                 GAS5UPD 
02599 ******************************************************************GAS5UPD 
02600  5000-000-READ-PROD-ALL-LVL-TAB  SECTION.                         GAS5UPD 
02601                                                                   GAS5UPD 
02602      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   GAS5UPD 
02603               GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACP-FIXED-LEN  +GAS5UPD 
02604              (GC-GCTABULR-ACP-VARY-LEN  *                         GAS5UPD 
02605                                    GC-GCTABULR-ACP-VARY-MAX-OCUR).GAS5UPD 
02606                                                                   GAS5UPD 
02607      IF ACWA-PR-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS5UPD 
02608         NEXT SENTENCE                                             GAS5UPD 
02609      ELSE                                                         GAS5UPD 
02610         EXEC CICS GETMAIN                                         GAS5UPD 
02611                SET(ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD)      GAS5UPD 
02612                INITIMG(WS-HEX-00)                                 GAS5UPD 
02613                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS5UPD 
02614                END-EXEC                                           GAS5UPD 
02615         SET ACWA-PR-ALL-LEVEL-TAB-PNTR     TO                     GAS5UPD 
02616                  ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD.        GAS5UPD 
02617                                                                   GAS5UPD 
02618      MOVE GCIO-WORKFILE-KEY       TO WS-SV-RESTO-KY.              GAS5UPD 
02619      MOVE TABIDI                  TO  GCIO-TAB-TABULAR-ID.        GAS5UPD 
02620      MOVE WRK-TAB-PROV-COPY-SLOT  TO  GCIO-TAB-SLOT-NO.           GAS5UPD 
02621      MOVE GCIO-WORKFILE-KEY       TO  GCIOA-FILE-KEY.             GAS5UPD 
02622      MOVE GC-GCIO-AREA-2          TO  GCIOA-IO-AREA-TO-USE.       GAS5UPD 
02623      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIOA-FILE-ACCESS-CODE.     GAS5UPD 
02624      MOVE GC-GCTABULR-DDNAME      TO  GCIOA-FILE-DDNAME.          GAS5UPD 
02625      MOVE GC-GCTABULR-ACP-VARY-MAX-OCUR                           GAS5UPD 
02626           TO  GAF2-ENTRY-COUNT.                                   GAS5UPD 
02627                                                                   GAS5UPD 
02628      MOVE WS-SV-RESTO-KY    TO  GCIO-WORKFILE-KEY.                GAS5UPD 
02629                                                                   GAS5UPD 
02630      IF GCIO-TAB-TABULAR-ID  =  GAF2-PROVISION-ID AND             GAS5UPD 
02631         GAF2-PROVISION-SLOT-NO  NUMERIC AND                       GAS5UPD 
02632         GCIO-TAB-SLOT-NO     =  GAF2-PROVISION-SLOT-NO            GAS5UPD 
02633         GO TO 5000-900-EXIT.                                      GAS5UPD 
02634                                                                   GAS5UPD 
02635      EXEC CICS  LINK   PROGRAM  ('GCIOPGM')                       GAS5UPD 
02636                 COMMAREA (PR-IO-PARM-ALL-LVL-TAB-RECORD)          GAS5UPD 
02637                 LENGTH (WS-IO-PARM-WRK-ALL-LVL-LEN)    END-EXEC.  GAS5UPD 
02638                                                                   GAS5UPD 
02639      IF NOT GCIOA-GOOD-RETURN                                     GAS5UPD 
02640         MOVE WS-ABCODE-1PF7        TO  WS-ABCODE                  GAS5UPD 
02641         MOVE WS-ABCODE-1PF7-MSG    TO  WS-ABCODE-MSG              GAS5UPD 
02642         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS5UPD 
02643                                                                   GAS5UPD 
02644  5000-900-EXIT.     EXIT.                                         GAS5UPD 
02645                                                                   GAS5UPD 
02646                                                                   GAS5UPD 
02647 /*****************************************************************GAS5UPD 
02648 * 6000  BUILD GROUP SPEC KEY                                     *GAS5UPD 
02649 *                                                                *GAS5UPD 
02650 *    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *GAS5UPD 
02651 ******************************************************************GAS5UPD 
02652  6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          GAS5UPD 
02653  6000-010.                                                        GAS5UPD 
02654                                                                   GAS5UPD 
02655      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS5UPD 
02656      MOVE  'G'                 TO  GCIO-WRK-STATUS-CODE.          GAS5UPD 
02657      MOVE  'G3'                TO  GCIO-WRK-RECORD-TYPE.          GAS5UPD 
02658      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS5UPD 
02659      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS5UPD 
02660      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS5UPD 
02661      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS5UPD 
02662      MOVE SPACES               TO  GCIO-WRK-LINE-OF-BUS,          GAS5UPD 
02663                                    GCIO-WRK-PROVIDER-CONTROL.     GAS5UPD 
02664      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS5UPD 
02665      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS5UPD 
02666      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID.         GAS5UPD 
02667      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS5UPD 
02668      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS5UPD 
02669      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS5UPD 
02670      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS5UPD 
02671                                                                   GAS5UPD 
02672  6000-900-EXIT. EXIT.                                             GAS5UPD 
02673                                                                   GAS5UPD 
02674 ******************************************************************GAS5UPD 
02675 * 6100  BUILD CONTRACT KEY                                       *GAS5UPD 
02676 *                                                                *GAS5UPD 
02677 *    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *GAS5UPD 
02678 ******************************************************************GAS5UPD 
02679  6100-000-BUILD-CONTRACT-KEY    SECTION.                          GAS5UPD 
02680  6100-010.                                                        GAS5UPD 
02681                                                                   GAS5UPD 
02682      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS5UPD 
02683      MOVE  'C'                 TO  GCIO-WRK-STATUS-CODE.          GAS5UPD 
02684      MOVE  'C3'                TO  GCIO-WRK-RECORD-TYPE.          GAS5UPD 
02685      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS5UPD 
02686      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS5UPD 
02687      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS5UPD 
02688      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS5UPD 
02689      MOVE GCA-L-O-B            TO  GCIO-WRK-LINE-OF-BUS.          GAS5UPD 
02690      MOVE GCA-PROV-CTL         TO  GCIO-WRK-PROVIDER-CONTROL.     GAS5UPD 
02691      MOVE GCA-FAM-REL-LVL      TO  GCIO-WRK-FAMILY-RELATION-LVL.  GAS5UPD 
02692      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS5UPD 
02693      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID.         GAS5UPD 
02694      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS5UPD 
02695      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS5UPD 
02696      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS5UPD 
02697      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS5UPD 
02698                                                                   GAS5UPD 
02699  6100-900-EXIT. EXIT.                                             GAS5UPD 
02700                                                                   GAS5UPD 
02701 /*****************************************************************GAS5UPD 
02702 * 6200  BUILD BEN PROV KEY                                       *GAS5UPD 
02703 *                                                                *GAS5UPD 
02704 *    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *GAS5UPD 
02705 ******************************************************************GAS5UPD 
02706  6200-000-BUILD-BEN-PROV-KEY    SECTION.                          GAS5UPD 
02707  6200-010.                                                        GAS5UPD 
02708                                                                   GAS5UPD 
02709      MOVE SPACES                TO  GCIO-WORKFILE-KEY.            GAS5UPD 
02710      MOVE  'C'                  TO  GCIO-WRK-STATUS-CODE.         GAS5UPD 
02711      MOVE  'C5'                 TO  GCIO-WRK-RECORD-TYPE.         GAS5UPD 
02712      MOVE GCA-PLAN-CODE         TO  GCIO-WRK-PLAN-CODE.           GAS5UPD 
02713      MOVE GCA-GROUP-NUM         TO  GCIO-WRK-GROUP-NUM.           GAS5UPD 
02714      MOVE GCA-SECTION-NUM       TO  GCIO-WRK-SECTION-NUM.         GAS5UPD 
02715      MOVE GCA-PKG-CODE          TO  GCIO-WRK-PKG-CODE.            GAS5UPD 
02716      MOVE GCA-L-O-B             TO  GCIO-WRK-LINE-OF-BUS.         GAS5UPD 
02717      MOVE GCA-PROV-CTL          TO  GCIO-WRK-PROVIDER-CONTROL.    GAS5UPD 
02718      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS5UPD 
02719      MOVE GCA-EFFDT-CEN         TO  GCIO-WRK-EFFDT-CEN.           GAS5UPD 
02720      MOVE GCA-BEN-PROV-ID       TO  GCIO-WRK-PROVISION-ID.        GAS5UPD 
02721      MOVE +9999999              TO  GCIO-WRK-PROVISION-SLOT-NO.   GAS5UPD 
02722      MOVE TABIDI                TO  GCIO-WRK-TAB-PROVISION-ID.    GAS5UPD 
02723      MOVE TABSLTNI              TO  ACWA-DISPLAY-LEN-7.           GAS5UPD 
02724      MOVE ACWA-DISPLAY-LEN-7    TO  GCIO-WRK-TAB-PROV-SLOT-NO.    GAS5UPD 
02725                                                                   GAS5UPD 
02726  6200-900-EXIT. EXIT.                                             GAS5UPD 
02727                                                                   GAS5UPD 
02728 /*****************************************************************GAS5UPD 
02729 *  XCTL TO MAIN MENU                                             *GAS5UPD 
02730 *                                                                *GAS5UPD 
02731 *    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *GAS5UPD 
02732 *  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*GAS5UPD 
02733 *  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUSGAS5UPD 
02734 *  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GETGAS5UPD 
02735 *  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *GAS5UPD 
02736 ******************************************************************GAS5UPD 
02737                                                                   GAS5UPD 
02738  6400-000-XCTL-TO-MAIN-MENU     SECTION.                          GAS5UPD 
02739  6400-010.                                                        GAS5UPD 
02740                                                                   GAS5UPD 
02741      MOVE WS-ABCODE-1PP1       TO  WS-ABCODE.                     GAS5UPD 
02742      MOVE WS-ABCODE-1PP1-MSG   TO  WS-ABCODE-MSG.                 GAS5UPD 
02743                                                                   GAS5UPD 
02744      EXEC CICS  XCTL   PROGRAM('GCPSPGM')   END-EXEC.             GAS5UPD 
02745                                                                   GAS5UPD 
02746  6400-900-EXIT. EXIT.                                             GAS5UPD 
02747                                                                   GAS5UPD 
02748 **************************************************************    GAS5UPD 
02749 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS5UPD 
02750 * - DEAD CODE ELIMINATION.                                        GAS5UPD 
02751 * - REMOVED: 7000-000-PRINT-HARDCOPY        SECTION.              GAS5UPD 
02752 **************************************************************    GAS5UPD 
02753                                                                   GAS5UPD 
02754 /*****************************************************************GAS5UPD 
02755 * 7900  RESET ATTRIBUTES                                         *GAS5UPD 
02756 ******************************************************************GAS5UPD 
02757  7900-000-RESET-ATTRIBUTES      SECTION.                          GAS5UPD 
02758  7900-010.                                                        GAS5UPD 
02759                                                                   GAS5UPD 
02760      MOVE DFHBMUNF  TO  BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA    GAS5UPD 
02761               LOBA      PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA    GAS5UPD 
02762               MANAPLIA  ASCDSCDA  INTRVALA  INTTYPEA  CLMLVLIA    GAS5UPD 
02763               BNMXVALA  DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA    GAS5UPD 
02764               FYIVALA   CONDALLA  CONDEXCA  CONDICDA  CONDTABA    GAS5UPD 
02765               CONDMENA  CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA    GAS5UPD 
02766               CONDMALA  CONDCARA  CONDOBSA  CONDKDYA  CONDACCA    GAS5UPD 
02767               CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA  BISNDINA    GAS5UPD 
02768         CONDPECA  CONDNEMA  DEFINTNA  CONDSUIA CONDTMJA CONDINFA  GAS5UPD 
02769               IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA    GAS5UPD 
02770               AGEQLLA   AGEQLHA   CONDLIFA  TIMEDOLA  IPGSOPTA    GAS5UPD 
02771               IDGDOPTA  IPGPOPTA  RELPINDA  AGELIMLA AGELIMHA     GAS5UPD 
02772               FEAKINDA  ACCUMIDA  CAPINDA   SABDINDA              GAS5UPD 
02772               BENTYPA   TIERCDA   TIERLVA.                        GAS5UPD 
02773                                                                   GAS5UPD 
02774                                                                   GAS5UPD 
02775      IF DELADDO  =  'CHG/DEL'                                     GAS5UPD 
02776         NEXT SENTENCE                                             GAS5UPD 
02777      ELSE                                                         GAS5UPD 
02778         GO TO 7900-900-EXIT.                                      GAS5UPD 
02779                                                                   GAS5UPD 
02780                                                                   GAS5UPD 
02781      IF CDEINDO  =  '+CDE+'                                       GAS5UPD 
02782 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS5UPD 
02783         MOVE DFHBMABF TO  DLOPTLTA  PERIOTA   BENVLQTA  LOTA      GAS5UPD 
02784                AGELIMA    PLCTRTTA  FAMINDTA  SRVGRUTA  CSTCOTTA  GAS5UPD 
02785                AGEQLTA    COPAYITA  INTDESTA  CONDTG1A  CONDTG2A  GAS5UPD 
                     BISNDITA                                                   
02786      ELSE                                                         GAS5UPD 
02787         NEXT SENTENCE.                                            GAS5UPD 
02788                                                                   GAS5UPD 
02789                                                                   GAS5UPD 
02790      IF CDEINDO  =  '+CDE-'                                       GAS5UPD 
02791 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS5UPD 
02792         MOVE DFHBMABF TO  DLOPTLTA  PERIOTA   BENVLQTA  LOTA      GAS5UPD 
02793                AGELIMA    PLCTRTTA  FAMINDTA  SRVGRUTA  CSTCOTTA  GAS5UPD 
02794                AGEQLTA    COPAYITA  INTDESTA  CONDTG1A  CONDTG2A  GAS5UPD 
02795                           DEFINTTA  BISNDITA                      GAS5UPD 
02796 *---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        GAS5UPD 
02797         MOVE DFHBMASF TO  DELOPTNA  PERIODA   BENVLQLA  LOBA      GAS5UPD 
02798         AGELIMLA AGELIMHA PLCTRMTA  FAMINDIA  SRVGRUPA  CSTCONTA  GAS5UPD 
02799         AGEQLLA  AGEQLHA  COPAYINA  INTDESKA  CONDALLA  CONDEXCA  GAS5UPD 
02800                           CONDICDA  CONDTABA  CONDMENA  CONDDRGA  GAS5UPD 
02801                 CONDLIFA  CONDALCA  CONDOBCA  CONDOBNA  CONDMALA  GAS5UPD 
02802                 CONDEACA  CONDEMCA  CONDSMIA  CONDNSMA  BISNDINA  GAS5UPD 
02803                 CONDTMJA  CONDCARA  CONDOBSA  CONDKDYA  CONDACCA  GAS5UPD 
02804                 CONDINFA  CONDPECA  CONDNEMA  CONDSUIA  DEFINTNA  GAS5UPD 
                                                                                
02805          IF  ERRMSGO  >  SPACES                                   GAS5UPD 
02806          THEN                                                     GAS5UPD 
02807              NEXT SENTENCE                                        GAS5UPD 
02808          ELSE                                                     GAS5UPD 
02809              SET  WT-01-INDEX  TO  +08                            GAS5UPD 
02810              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO    GAS5UPD 
02811      ELSE                                                         GAS5UPD 
02812          NEXT SENTENCE.                                           GAS5UPD 
02813                                                                   GAS5UPD 
02814  7900-900-EXIT. EXIT.                                             GAS5UPD 
02815                                                                   GAS5UPD 
02816 /*****************************************************************GAS5UPD 
02817 * 9000  SEND ERASE THEN RETURN                                   *GAS5UPD 
02818 ******************************************************************GAS5UPD 
02819  9000-000-SEND-ERASE-RETURN     SECTION.                          GAS5UPD 
02820  9000-010.                                                        GAS5UPD 
02821                                                                   GAS5UPD 
02822      MOVE DFHBMASD  TO                                            GAS5UPD 
02823                    PERLITTA REININTA MAXOVRTA FDLRCLTA            GAS5UPD 
02824                    PERLIMTA REININDA MAXOVRDA FDLRCLIA            GAS5UPD 
02825                    CARYOVRA.                                      GAS5UPD 
02826      MOVE -1  TO  ERRMSGL.                                        GAS5UPD 
02827                                                                   GAS5UPD 
02828      EXEC CICS  SEND   MAP ('GA1XI01')    ERASE  CURSOR           GAS5UPD 
02829                 MAPSET('GA1XSET')    END-EXEC.                    GAS5UPD 
02830                                                                   GAS5UPD 
02831      EXEC CICS  RETURN   END-EXEC.                                GAS5UPD 
02832                                                                   GAS5UPD 
02833  9000-900-EXIT.    EXIT.                                          GAS5UPD 
02834                                                                   GAS5UPD 
02835 /*****************************************************************GAS5UPD 
02836 * 9010  SEND DATAONLY AND RETURN                                 *GAS5UPD 
02837 ******************************************************************GAS5UPD 
02838  9010-000-SEND-DATAONLY-RETURN  SECTION.                          GAS5UPD 
02839  9010-010.                                                        GAS5UPD 
02840                                                                   GAS5UPD 
02841      MOVE -1  TO  ERRMSGL.                                        GAS5UPD 
02842                                                                   GAS5UPD 
02843      EXEC CICS  SEND   MAP('GA1XI01')  DATAONLY  CURSOR           GAS5UPD 
02844                 MAPSET('GA1XSET')       END-EXEC.                 GAS5UPD 
02845                                                                   GAS5UPD 
02846      EXEC CICS  RETURN   END-EXEC.                                GAS5UPD 
02847                                                                   GAS5UPD 
02848  9010-900-EXIT.     EXIT.                                         GAS5UPD 
02849                                                                   GAS5UPD 
02850 **************************************************************    GAS5UPD 
02851 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS5UPD 
02852 * - DEAD CODE ELIMINATION.                                        GAS5UPD 
02853 * - REMOVED: 9200-000-GREGORIAN-TO-JULIAN   SECTION.              GAS5UPD 
02854 *            9300-000-JULIAN-TO-GREGORIAN   SECTION.              GAS5UPD 
02855 **************************************************************    GAS5UPD 
02856                                                                   GAS5UPD 
02857 /*****************************************************************GAS5UPD 
02858 * 9800  E R R O R   T H E N   A B E N D                          *GAS5UPD 
02859 *                                                                *GAS5UPD 
02860 *    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *GAS5UPD 
02861 *  AND THEN ABENDS USING THE ABEND CODE EARLIER MEFINED.         *GAS5UPD 
02862 ******************************************************************GAS5UPD 
02863  9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          GAS5UPD 
02864  9800-010.                                                        GAS5UPD 
02865                                                                   GAS5UPD 
02866      MOVE -1              TO  MFRMSLTL.                           GAS5UPD 
02867      MOVE WS-ABCODE-MSG   TO  ERRMSGO.                            GAS5UPD 
02868                                                                   GAS5UPD 
02869      EXEC CICS  SEND   MAP ('GA1XI01') ERASE  CURSOR   WAIT       GAS5UPD 
02870                 MAPSET('GA1XSET')      END-EXEC.                  GAS5UPD 
02871                                                                   GAS5UPD 
02872      EXEC CICS  ABEND   ABCODE(WS-ABCODE)   END-EXEC.             GAS5UPD 
02873                                                                   GAS5UPD 
02874  9800-900-EXIT. EXIT.                                             GAS5UPD 
