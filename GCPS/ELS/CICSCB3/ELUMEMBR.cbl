00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.    ELUMEMBR.                                         ELUMEMBR
00003  AUTHOR.        RICK BARILEAU.                                       LV002
00004  INSTALLATION.  HEALTH CARE SERVICE CORPORATION                   ELUMEMBR
00005                 A MUTUAL LEGAL RESERVE COMPANY                    ELUMEMBR
00006                 BLUE CROSS/BLUE SHIELD OF ILLINOIS                ELUMEMBR
00007                 233 N. MICHIGAN AVE                               ELUMEMBR
00008                 CHICAGO, ILLINOIS 60601                           ELUMEMBR
00009  DATE-WRITTEN.                                                    ELUMEMBR
00010                                                                   ELUMEMBR
00011  ENVIRONMENT DIVISION.                                            ELUMEMBR
00012                                                                   ELUMEMBR
00013  CONFIGURATION SECTION.                                           ELUMEMBR
00014  SOURCE-COMPUTER.    IBM-3033.                                    ELUMEMBR
00015  OBJECT-COMPUTER.    IBM-3033.                                    ELUMEMBR
00016                                                                   ELUMEMBR
00017 ***************************************************************   ELUMEMBR
00018 *                                                             *   ELUMEMBR
00019 *  ELUMEMBR  -                                                *   ELUMEMBR
00020 *                                                             *   ELUMEMBR
00021 *                                                             *   ELUMEMBR
00022 *                                                             *   ELUMEMBR
00023 ***************************************************************   ELUMEMBR
00024 *                                                             *   ELUMEMBR
00025 *                      MAINTENANCE HISTORY                    *   ELUMEMBR
00026 *                                                             *   ELUMEMBR
00027 *  MOD     DATE      BY  DRPT              ACTION             *   ELUMEMBR
00028 * ----- ----------- --- ----- ------------------------------- *   ELUMEMBR
00029 * 01.00 23-MAY-1988 REB       CREATED (REWRITTEN WITH         *   ELUMEMBR
00030 *                             ADDITIONAL SPECS GIVEN).        *   ELUMEMBR
00031 *                                                             *   ELUMEMBR
00032 * 01.01 01-JUN-1988 REB       RESTORING TWA AREA IN AN ABEND  *   ELUMEMBR
00033 *                             SITUATION AND AFTER ALL         *   ELUMEMBR
00034 *                             PROCESSING.                     *   ELUMEMBR
00035 *                                                                 ELUMEMBR
00036 *01.02  31-MAR-1989 AKK       DESTRUCT PROGRAM PREVIOUSLY IN  *   ELUMEMBR
00037 *                             STRUCTURES                      *   ELUMEMBR
00038 *                                                                 ELUMEMBR
00039 *01.03  13-APR-1989 GEM       STORAGE MANAGEMENT ENHANCEMENTS *   ELUMEMBR
00040 *                                                                 ELUMEMBR
00041 *01.04  17-JAN-1990 EGL       CORRECTED PROBLEM WITH TWA      *   ELUMEMBR
00042 *                             COMMAREA POINTER BEING CLOBBERED    ELUMEMBR
00043 *                             BY CSEXEXIO                         ELUMEMBR
00044 *                                                                 ELUMEMBR
00045 *01.05  08-APR-L997 AKK       ADDED EL62 WHEN IO ERROR        *   ELUMEMBR
00046 *                             FOUND.  THIS WAS CAUSING GCD01B     ELUMEMBR
00047 *                             TO ABEND.                           ELUMEMBR
00048 *                                                                 ELUMEMBR
00049 *01.06  06-NOV-1997 KJD       ADDED TEXAS PACKAGE CODE LOOKUP *   ELUMEMBR
00050 *                             AND TEMPORARY SECTION SWAP          ELUMEMBR
00051 *                                                                 ELUMEMBR
00052 *01.07  07-NOV-1997 AKK       ADD SUPPORT FOR YR2000 AND TX   *   ELUMEMBR
00053 *                             MERGR.  WILL REMOVE TEMP SWAP   *   ELUMEMBR
00054 *                                                             *   ELUMEMBR
00055 *01.08  25-FEB-2000 JP        CHANGED INFINITY DATES TO       *   ELUMEMBR
00056 *                             12/31/9999.  CHANGED WS HOLD    *   ELUMEMBR
00057 *                             SECT FROM PIC 9 TO X.           *   ELUMEMBR
00058 *                                                             *   ELUMEMBR
00059 *01.09  26-SEP-2000 AKK       CHANGED MSC4805H TO MSC4805X   *    ELUMEMBR
00060 *                             AS RECEVING COMPILING ERRORS!   *   ELUMEMBR
00061 *                             THIS IS THE SECOND TIME WE HAVE *   ELUMEMBR
00062 *                             HAD TO CHANGE THE NAME OF THIS  *   ELUMEMBR
00063 *                             COPY MEMBER.                    *   ELUMEMBR
00064 *                                                             *   ELUMEMBR
00065 *01.10  19-DEC-2002 AKK       RECOMPILING TO CHECK OUT ASRA.  *   ELUMEMBR
00066 ***************************************************************   ELUMEMBR
00067                                                                   ELUMEMBR
00068  DATA DIVISION.                                                   ELUMEMBR
00069                                                                   ELUMEMBR
00070  FILE SECTION.                                                    ELUMEMBR
00071                                                                   ELUMEMBR
00072  WORKING-STORAGE SECTION.                                         ELUMEMBR
00073  01  WS-MISC.                                                     ELUMEMBR
00074      05  FILLER                  PIC  X(37)     VALUE             ELUMEMBR
00075          '** THE WS FOR ELUMEMBR STARTS HERE **'.                 ELUMEMBR
00076      05  WS-ACTION-MODULE             PIC X(8) VALUE SPACES.      ELUMEMBR
00077      05  WS-TXSUP-DDNAME              PIC X(8) VALUE 'SMFSUPLV'.  ELUMEMBR
00078      05  WS-TXSUP-RESP1               PIC S9(8) COMP VALUE +0.    ELUMEMBR
00079      05  WS-PKG-SUB1                  PIC 9(3)  VALUE 0.          ELUMEMBR
00080      05  WS-SUB                       PIC 9(3)  VALUE 0.          ELUMEMBR
00081                                                                   ELUMEMBR
00082      05  WS-TXSUP-KEY.                                            ELUMEMBR
00083          10  WS-TXSUP-GROUP           PIC X(06)  VALUE SPACES.    ELUMEMBR
00084          10  WS-TXSUP-SECT            PIC X(04)  VALUE SPACES.    ELUMEMBR
00085          10  WS-TXSUP-SUB-NUMBER      PIC X(12)  VALUE SPACES.    ELUMEMBR
00086          10  WS-TXSUP-MEM-NUMBER      PIC 9(02) VALUE ZEROES.     ELUMEMBR
00087              88 WS-TXSUP-SUBSCRIBER   VALUE 0.                    ELUMEMBR
00088          10  WS-TXSUP-REC-TYPE        PIC X(02)  VALUE SPACES.    ELUMEMBR
00089              88  WS-TXSUP-PCP         VALUE '00'.                 ELUMEMBR
00090              88  WS-TXSUP-HMO         VALUE '01'.                 ELUMEMBR
00091              88  WS-TXSUP-TEX         VALUE '10'.                 ELUMEMBR
00092          10  FILLER                   PIC X(15)  VALUE SPACES.    ELUMEMBR
00093                                                                   ELUMEMBR
00094  01  WS-CHECK-SECT.                                               ELUMEMBR
00095      05 FILLER                   PIC X(03).                       ELUMEMBR
00096      05 WS-SECT-LAST-BYTE        PIC X(01).                       ELUMEMBR
00097                                                                   ELUMEMBR
00098 * TEXAS REGIONS FOR PACKAGE CODE CHECK                            ELUMEMBR
00099  01  WS-APPLID.                                                   ELUMEMBR
00100      02 FILLER                   PIC X(03).                       ELUMEMBR
00101      02 FILLER                   PIC X(04).                       ELUMEMBR
00102         88 TEXAS-REGION          VALUES                           ELUMEMBR
00103         'XAI1' 'XAI2' 'XAB1' 'XAB2' 'XAB3' 'XAB4' 'XAB5'          ELUMEMBR
00104         'XAB6' 'XAB7' 'XAB8' 'XAB9' 'XAS1' 'XAS2' 'XFB1'          ELUMEMBR
00105         'XFB2' 'XF01'.                                            ELUMEMBR
00106                                                                   ELUMEMBR
00107  01  WS-GROUPS-TO-SKIP.                                           ELUMEMBR
00108      02 WS-CHANGE-TX-GROUP-CHECK PIC X(06).                       ELUMEMBR
00109         88  WS-CHANGE-TX-PKG-CODE-GROUP                           ELUMEMBR
00110              VALUES '0FEPTX' '051200' '051201'                    ELUMEMBR
00111                     '051300' '051301' '000600'                    ELUMEMBR
00112                     '061100' '061500' '061600'                    ELUMEMBR
00113                     '071100'.                                     ELUMEMBR
00114                                                                   ELUMEMBR
00115                                                                   ELUMEMBR
00116  01 WS-485-HOLD-CC-YY.                                            ELUMEMBR
00117      10  WS-485-HOLD-CC        PIC 9(02).                         ELUMEMBR
00118      10  WS-485-HOLD-YY        PIC 9(02).                         ELUMEMBR
00119      10  WS-485-HOLD-MO        PIC 9(02).                         ELUMEMBR
00120      10  WS-485-HOLD-DA        PIC 9(02).                         ELUMEMBR
00121                                                                   ELUMEMBR
00122                                                                   ELUMEMBR
00123  01  WS-SWITCHES.                                                 ELUMEMBR
00124      05  WS-485-CONVERT-SW       PIC X(01)      VALUE SPACE.      ELUMEMBR
00125          88  WS-485-CONVERT                     VALUE 'C'.        ELUMEMBR
00126                                                                   ELUMEMBR
00127      05  WS-MEMB-BROWSE-SW       PIC X(01)      VALUE SPACE.      ELUMEMBR
00128          88  STOP-MEMB-BROWSE                   VALUE 'S'.        ELUMEMBR
00129                                                                   ELUMEMBR
00130      05  WS-PROCESS-BCKWRD-SW    PIC X(01)      VALUE SPACE.      ELUMEMBR
00131          88  STOP-BCKWRD-CHAIN                  VALUE 'B'.        ELUMEMBR
00132                                                                   ELUMEMBR
00133      05  WS-PROCESS-FWD-SW       PIC X(01)      VALUE SPACE.      ELUMEMBR
00134          88  STOP-FWD-CHAIN                     VALUE 'F'.        ELUMEMBR
00135                                                                   ELUMEMBR
00136      05  WS-PREV-KEY-SW          PIC X(01)      VALUE SPACE.      ELUMEMBR
00137          88  PREV-KEY-CHANGED                   VALUE 'P'.        ELUMEMBR
00138                                                                   ELUMEMBR
00139      05  WS-TRAN-TO-KEY-SW       PIC X(01)      VALUE SPACE.      ELUMEMBR
00140          88  TRANS-TO-KEY-CHANGES               VALUE 'T'.        ELUMEMBR
00141                                                                   ELUMEMBR
00142      05  WS-LOOP-SW              PIC X(01)      VALUE SPACE.      ELUMEMBR
00143          88  FILE-LOOP                          VALUE 'L'.        ELUMEMBR
00144                                                                   ELUMEMBR
00145      05  WS-MEM-PKG-FND-SW       PIC X(01)      VALUE SPACE.      ELUMEMBR
00146          88  WS-MEM-PKG-FND                     VALUE 'Y'.        ELUMEMBR
00147                                                                   ELUMEMBR
00148      05  WS-PKG-DONE-SW          PIC X(01)      VALUE SPACE.      ELUMEMBR
00149                                                                   ELUMEMBR
00150      05  WS-TXSUP-EOF-SW         PIC 9(01)      VALUE 0.          ELUMEMBR
00151                                                                   ELUMEMBR
00152      05  WS-TXSUP-BROWSE-SW      PIC 9(01)      VALUE 0.          ELUMEMBR
00153          88  TXSUP-BROWSE-SW-ON         VALUE 1.                  ELUMEMBR
00154                                                                   ELUMEMBR
00155  01  WS-WORK-FIELDS.                                              ELUMEMBR
00156      05  WS-CSEXECIO-LINK-COUNT  PIC S9(04)     VALUE +0  COMP.   ELUMEMBR
00157          88  MAXIMUM-READ-LIMIT                 VALUE +0050.      ELUMEMBR
00158      05  WS-HOLD-EFF-DT.                                          ELUMEMBR
00159          10  WS-HOLD-EFF-DATE-CC   PIC X.                         ELUMEMBR
00160          10  WS-HOLD-EFF-DATE   PIC S9(05)    VALUE +0  COMP-3.   ELUMEMBR
00161      05  WS-HOLD-EFF-DT-CEN  REDEFINES WS-HOLD-EFF-DT             ELUMEMBR
00162            PIC S9(07) COMP-3.                                     ELUMEMBR
00163      05  WS-HOLD-CAN-DT.                                          ELUMEMBR
00164          10  WS-HOLD-CAN-DATE-CC   PIC X.                         ELUMEMBR
00165          10  WS-HOLD-CAN-DATE   PIC S9(05)    VALUE +0  COMP-3.   ELUMEMBR
00166      05  WS-HOLD-CAN-DT-CEN  REDEFINES WS-HOLD-CAN-DT             ELUMEMBR
00167            PIC S9(07) COMP-3.                                     ELUMEMBR
00168      05  WS-HOLD-SECT-NO.                                         ELUMEMBR
00169          10  WS-HOLD-SECT-NO-1   PIC  X(01)     VALUE ZEROES.     ELUMEMBR
00170          10  WS-HOLD-SECT-NO-4   PIC  X(04)     VALUE SPACES.     ELUMEMBR
00171      05  WS-HOLD-PKG-CODE        PIC  X(03)     VALUE SPACES.     ELUMEMBR
00172      05  WS-MEMB-BROWSE          PIC S9(03)     VALUE +150.       ELUMEMBR
00173                                                                   ELUMEMBR
00174      05  WS-CSDTCONV-UNPACKED-DT.                                 ELUMEMBR
00175          10  WS-CSDTCONV-UNPACKED-DT-2  PIC X(02).                ELUMEMBR
00176          10  WS-CSDTCONV-UNPACKED-DT-6  PIC X(06).                ELUMEMBR
00177                                                                   ELUMEMBR
00178      05  WS-CSDTCONV-OPERATION    PIC X.                          ELUMEMBR
00179                                                                   ELUMEMBR
00180      05  WS-DATE-FIELD           PIC S9(06)V9   VALUE +0  COMP-3. ELUMEMBR
00181      05  WS-DATE-FIELD-1         REDEFINES WS-DATE-FIELD.         ELUMEMBR
00182          10  DATE-YMD            PIC X(03).                       ELUMEMBR
00183          10  FILLER              PIC X(01).                       ELUMEMBR
00184      05  WS-DATE-JUL             PIC S9(5)      VALUE ZEROES.     ELUMEMBR
00185                                                                   ELUMEMBR
00186      05  WS-SSB-SRV-FROM-DATE-YMD PIC 9(6)      VALUE ZEROES.     ELUMEMBR
00187      05  WS-SSB-SRV-TO-DATE-YMD   PIC 9(6)      VALUE ZEROES.     ELUMEMBR
00188                                                                   ELUMEMBR
00189      05  WS-485-JUL-EFF        PIC S9(07) COMP-3 VALUE ZEROES.    ELUMEMBR
00190      05  WS-485-JUL-END        PIC S9(07) COMP-3 VALUE ZEROES.    ELUMEMBR
00191                                                                   ELUMEMBR
00192      05  WS-HOLD-TRANSFER-KEY.                                    ELUMEMBR
00193          10  HOLD-TRNSFR-GRP-6   PIC X(06)      VALUE SPACES.     ELUMEMBR
00194          10  HOLD-TRNSFR-SCT-4   PIC X(04)      VALUE SPACES.     ELUMEMBR
00195          10  HOLD-TRNSFR-MBR     PIC X(12)      VALUE SPACES.     ELUMEMBR
00196                                                                   ELUMEMBR
00197  01  SUBX                        PIC S9(04)     VALUE +0  COMP.   ELUMEMBR
00198  01  SUBY                        PIC S9(04)     VALUE +0  COMP.   ELUMEMBR
00199  01  WS-MBR-ENTRY-CNT            PIC S9(04)     VALUE +0  COMP-3. ELUMEMBR
00200  01  WS-SECTION-ENTRIES          PIC S9(04)     VALUE +0  COMP-3. ELUMEMBR
00201  01  WS-TWA-PTR                  POINTER        VALUE NULL.       ELUMEMBR
00202  01  WS-TWA-HOLD-PTR             POINTER        VALUE NULL.       ELUMEMBR
00203                                                                   ELUMEMBR
00204 **************************************************************    ELUMEMBR
00205 ***  THE WS-MEMB-INFO HAS ALL THE VALID ENTRIES THAT WILL  ***    ELUMEMBR
00206 ***  BE MADE AVAILABLE.                                    ***    ELUMEMBR
00207 **************************************************************    ELUMEMBR
00208  01  WS-MEMB-TABLE-AREA.                                          ELUMEMBR
00209      05  WS-MEMB-INFO            OCCURS 20 TIMES                  ELUMEMBR
00210                                  INDEXED BY WS-MBR-IDX.           ELUMEMBR
00211          10  WS-MBR-SCT-NBR.                                      ELUMEMBR
00212             15  WS-MBR-SCT-NBR-1    PIC  X(01) VALUE ZEROES.      ELUMEMBR
00213             15  WS-MBR-SCT-NBR-4    PIC  X(04).                   ELUMEMBR
00214          10 WS-MBR-PKG-CODE         PIC  X(03).                   ELUMEMBR
00215          10  WS-MBR-EFF-DATE.                                     ELUMEMBR
00216             15  WS-MBR-EFF-DATE-CC  PIC X.                        ELUMEMBR
00217             15  WS-MBR-EFF-DT       PIC S9(05) COMP-3.            ELUMEMBR
00218          10  WS-MBR-EFF-DATE-CEN REDEFINES WS-MBR-EFF-DATE        ELUMEMBR
00219                    PIC S9(07) COMP-3.                             ELUMEMBR
00220          10  WS-MBR-CAN-DATE.                                     ELUMEMBR
00221             15  WS-MBR-CAN-DATE-CC  PIC X.                        ELUMEMBR
00222             15  WS-MBR-CAN-DT       PIC S9(05) COMP-3.            ELUMEMBR
00223          10  WS-MBR-CAN-DATE-CEN REDEFINES WS-MBR-CAN-DATE        ELUMEMBR
00224                    PIC S9(07) COMP-3.                             ELUMEMBR
00225                                                                   ELUMEMBR
00226 **************************************************************    ELUMEMBR
00227 ***  THE WS-LAST-MEMB-INFO CONTAINS ALL ENTRIES WHERE THE         ELUMEMBR
00228 ***  INQUIRY IS BEYOND ANY MEMBERSHIP RECORDS. THIS WILL ALSO     ELUMEMBR
00229 ***  HELP DETERMINE IF THE FORWARD CHAIN IS LOOPING.              ELUMEMBR
00230 **************************************************************    ELUMEMBR
00231  01  WS-LAST-RECORD-AREA.                                         ELUMEMBR
00232      05  WS-LAST-SCT.                                             ELUMEMBR
00233          10  WS-LAST-SCT-1       PIC  X(01)     VALUE ZEROES.     ELUMEMBR
00234          10  WS-LAST-SCT-4       PIC  X(04)     VALUE SPACES.     ELUMEMBR
00235      05  WS-LAST-EFF-DATE.                                        ELUMEMBR
00236          10  WS-LAST-EFF-DATE-CC  PIC X.                          ELUMEMBR
00237          10  WS-LAST-EFF-DATE     PIC S9(05)               COMP-3.ELUMEMBR
00238      05  WS-LAST-EFF-DATE-CEN REDEFINES WS-LAST-EFF-DATE          ELUMEMBR
00239                    PIC S9(07) COMP-3.                             ELUMEMBR
00240      05  WS-LAST-CAN-DATE.                                        ELUMEMBR
00241          10  WS-LAST-CAN-DATE-CC  PIC X.                          ELUMEMBR
00242          10  WS-LAST-CAN-DATE     PIC S9(05)               COMP-3.ELUMEMBR
00243      05   WS-LAST-CAN-DATE-CEN REDEFINES WS-LAST-CAN-DATE         ELUMEMBR
00244                    PIC S9(07) COMP-3.                             ELUMEMBR
00245                                                                   ELUMEMBR
00246  01  WS-ALL-SECTIONS-READ-TABLE.                                  ELUMEMBR
00247      05  WS-SECTION-ENTRY        OCCURS 50 TIMES                  ELUMEMBR
00248                                  INDEXED BY WS-SCT-IDX.           ELUMEMBR
00249          10  WS-SECTION-NUMBER.                                   ELUMEMBR
00250              15  WS-SECTION-NBR-1    PIC  X(01).                  ELUMEMBR
00251              15  WS-SECTION-NBR      PIC  X(04).                  ELUMEMBR
00252          10  WS-PKG-CODE             PIC  X(03).                  ELUMEMBR
00253                                                                   ELUMEMBR
00254 * HEX COBOAL VALUES                                               ELUMEMBR
00255   COPY HEXCOBOL.                                                  ELUMEMBR
00256                                                                   ELUMEMBR
00257 * TEXAS SMF SUPPLEMENTAL FILE                                     ELUMEMBR
00258   COPY MSC4805X.                                                  ELUMEMBR
00259                                                                   ELUMEMBR
00260  01  WS-PKG-CODE-TABLE.                                           ELUMEMBR
00261 ** PKG CODE TBL FOR TEXAS **                                      ELUMEMBR
00262  COPY GCPKGTBL.                                                   ELUMEMBR
00263                                                                   ELUMEMBR
00264 ** LIST OF GROUPS TO SKIP FOR PACKGE CODE PROCESS **              ELUMEMBR
00265  COPY GCPKGSKP.                                                   ELUMEMBR
00266                                                                   ELUMEMBR
00267 * MILLENIUM CONVERSION COPY MEMBER                                ELUMEMBR
00268      COPY MLDATE01.                                               ELUMEMBR
00269                                                                   ELUMEMBR
00270  01  HGADATES-PARM-LIST.                                          ELUMEMBR
00271      COPY HGCDAT01.                                               ELUMEMBR
00272 /                                                                 ELUMEMBR
00273  LINKAGE SECTION.                                                 ELUMEMBR
00274  01  DFHCOMMAREA.                                                 ELUMEMBR
00275      COPY ELSCOMMC.                                               ELUMEMBR
00276 /                                                                 ELUMEMBR
00277      COPY ELSCIA2C.                                               ELUMEMBR
00278 /                                                                 ELUMEMBR
00279      COPY ELSSSCBC.                                               ELUMEMBR
00280 /                                                                 ELUMEMBR
00281      COPY ELSIOPMC.                                               ELUMEMBR
00282 /                                                                 ELUMEMBR
00283      COPY ELSMEMSC.                                               ELUMEMBR
00284 /                                                                 ELUMEMBR
00285      COPY ELSTWAC.                                                ELUMEMBR
00286 /                                                                 ELUMEMBR
00287  COPY COBXIO2.                                                    ELUMEMBR
00288 /                                                                 ELUMEMBR
00289  01  GROUP-HEADER.                                                ELUMEMBR
00290      COPY RDMC4306.                                               ELUMEMBR
00291 /                                                                 ELUMEMBR
00292  01  MEMBER-HEADER.                                               ELUMEMBR
00293      COPY RDMC4308.                                               ELUMEMBR
00294      COPY MSRDL484.                                               ELUMEMBR
00295 /                                                                 ELUMEMBR
00296  PROCEDURE DIVISION.                                              ELUMEMBR
00297 ************************************************************      ELUMEMBR
00298 *                                                          *      ELUMEMBR
00299 *                    PROCEDURE DIVISION                    *      ELUMEMBR
00300 *                                                          *      ELUMEMBR
00301 ************************************************************      ELUMEMBR
00302                                                                   ELUMEMBR
00303                                                                   ELUMEMBR
00304 ************************************************************      ELUMEMBR
00305 *                                                          *      ELUMEMBR
00306 *        OBTAIN MEMBERSHIP INFORMATION MAINLINE            *      ELUMEMBR
00307 *                                                          *      ELUMEMBR
00308 ************************************************************      ELUMEMBR
00309  OBTAIN-MEMBERSHIP-INFORMATIONX.                                  ELUMEMBR
00310      PERFORM INITIALIZATION.                                      ELUMEMBR
00311      PERFORM PROCESS.                                             ELUMEMBR
00312      GOBACK.                                                      ELUMEMBR
00313                                                                   ELUMEMBR
00314                                                                   ELUMEMBR
00315 ************************************************************      ELUMEMBR
00316 *                                                          *      ELUMEMBR
00317 *        INITIALIZATION                                    *      ELUMEMBR
00318 *                                                          *      ELUMEMBR
00319 ************************************************************      ELUMEMBR
00320  INITIALIZATION.                                                  ELUMEMBR
00321      PERFORM CHECK-COMMAREA-LENGTH.                               ELUMEMBR
00322      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELUMEMBR
00323      PERFORM GETMAIN-MEMBERSHIP-INFORMATION.                      ELUMEMBR
00324      PERFORM GETMAIN-CSEXECIO-CONTROL.                            ELUMEMBR
00325      INITIALIZE CIA-RETURN-CODE.                                  ELUMEMBR
00326      PERFORM INITIALIZE-WS-ALL-SECTIONS-REA                       ELUMEMBR
00327          VARYING WS-SCT-IDX FROM 1 BY 1                           ELUMEMBR
00328                  UNTIL   WS-SCT-IDX > 50.                         ELUMEMBR
00329      PERFORM INITIALIZE-BOTH-WORKING-STORAG                       ELUMEMBR
00330          VARYING WS-MBR-IDX FROM 1 BY 1                           ELUMEMBR
00331                  UNTIL   WS-MBR-IDX > 20.                         ELUMEMBR
00332      EXEC CICS ADDRESS                                            ELUMEMBR
00333                TWA (WS-TWA-PTR)                                   ELUMEMBR
00334         END-EXEC.                                                 ELUMEMBR
00335      SET ADDRESS OF TWA-TRANSACTION-WORK-AREA TO                  ELUMEMBR
00336          WS-TWA-PTR.                                              ELUMEMBR
00337                                                                   ELUMEMBR
00338      EXEC CICS ASSIGN APPLID (WS-APPLID) END-EXEC.                ELUMEMBR
00339                                                                   ELUMEMBR
00340      MOVE SSB-GRP-NO TO WS-CHANGE-TX-GROUP-CHECK.                 ELUMEMBR
00341                                                                   ELUMEMBR
00342 ************************************************************      ELUMEMBR
00343 *                                                          *      ELUMEMBR
00344 *        INITIALIZE WS-ALL-SECTIONS-READ-TABLE             *      ELUMEMBR
00345 *                                                          *      ELUMEMBR
00346 ************************************************************      ELUMEMBR
00347  INITIALIZE-WS-ALL-SECTIONS-REA.                                  ELUMEMBR
00348      MOVE LOW-VALUES TO WS-SECTION-NUMBER (WS-SCT-IDX).           ELUMEMBR
00349                                                                   ELUMEMBR
00350                                                                   ELUMEMBR
00351 ************************************************************      ELUMEMBR
00352 *                                                          *      ELUMEMBR
00353 *        INITIALIZE BOTH WORKING STORAGE TABLES            *      ELUMEMBR
00354 *                                                          *      ELUMEMBR
00355 ************************************************************      ELUMEMBR
00356  INITIALIZE-BOTH-WORKING-STORAG.                                  ELUMEMBR
00357      MOVE LOW-VALUES TO    WS-MBR-SCT-NBR                         ELUMEMBR
00358          (WS-MBR-IDX).                                            ELUMEMBR
00359      MOVE ZEROS      TO    WS-MBR-EFF-DATE  (WS-MBR-IDX)          ELUMEMBR
00360                            WS-MBR-CAN-DATE                        ELUMEMBR
00361          (WS-MBR-IDX).                                            ELUMEMBR
00362                                                                   ELUMEMBR
00363                                                                   ELUMEMBR
00364 ************************************************************      ELUMEMBR
00365 *                                                          *      ELUMEMBR
00366 *        CHECK COMMAREA LENGTH                             *      ELUMEMBR
00367 *                                                          *      ELUMEMBR
00368 ************************************************************      ELUMEMBR
00369  CHECK-COMMAREA-LENGTH.                                           ELUMEMBR
00370      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELUMEMBR
00371            EXEC CICS ABEND                                        ELUMEMBR
00372                  ABCODE('EL01')                                   ELUMEMBR
00373           END-EXEC.                                               ELUMEMBR
00374                                                                   ELUMEMBR
00375 ************************************************************      ELUMEMBR
00376 *                                                          *      ELUMEMBR
00377 *        SIGNAL IO ERROR                                   *      ELUMEMBR
00378 *                                                          *      ELUMEMBR
00379 ************************************************************      ELUMEMBR
00380  SIGNAL-IO-ERROR.                                                 ELUMEMBR
00381      MOVE 'EL62' TO CIA-ABCODE.                                   ELUMEMBR
00382      EXEC CICS ABEND                                              ELUMEMBR
00383                ABCODE(CIA-ABCODE)                                 ELUMEMBR
00384                END-EXEC.                                          ELUMEMBR
00385                                                                   ELUMEMBR
00386                                                                   ELUMEMBR
00387 ************************************************************      ELUMEMBR
00388 *                                                          *      ELUMEMBR
00389 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELUMEMBR
00390 *                                                          *      ELUMEMBR
00391 ************************************************************      ELUMEMBR
00392  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELUMEMBR
00393      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUMEMBR
00394          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUMEMBR
00395                                                                   ELUMEMBR
00396      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUMEMBR
00397      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUMEMBR
00398          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELUMEMBR
00399                                                                   ELUMEMBR
00400                                                                   ELUMEMBR
00401 ************************************************************      ELUMEMBR
00402 *                                                          *      ELUMEMBR
00403 *        GETMAIN MEMBERSHIP INFORMATION                    *      ELUMEMBR
00404 *                                                          *      ELUMEMBR
00405 ************************************************************      ELUMEMBR
00406  GETMAIN-MEMBERSHIP-INFORMATION.                                  ELUMEMBR
00407      SET CIA-ELSMEMS-DDN    TO TRUE.                              ELUMEMBR
00408      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUMEMBR
00409          ADDRESS OF MSI-MEMBERSHIP-INTERFACE.                     ELUMEMBR
00410      COMPUTE CIA-AREA-LEN = LENGTH OF MSI-MBR-INFO +              ELUMEMBR
00411             (CIA-MVO * LENGTH OF                                  ELUMEMBR
00412          MSI-MBR-SECN-TBL).                                       ELUMEMBR
00413      SET CIA-STG-GETMAIN    TO TRUE.                              ELUMEMBR
00414      PERFORM CALL-STORAGE-MANAGER.                                ELUMEMBR
00415      SET CIA-ELSMEMS-DDN TO TRUE.                                 ELUMEMBR
00416      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUMEMBR
00417          ADDRESS OF MSI-MEMBERSHIP-INTERFACE.                     ELUMEMBR
00418                                                                   ELUMEMBR
00419                                                                   ELUMEMBR
00420 ************************************************************      ELUMEMBR
00421 *                                                          *      ELUMEMBR
00422 *        CALL STORAGE MANAGER                              *      ELUMEMBR
00423 *                                                          *      ELUMEMBR
00424 ************************************************************      ELUMEMBR
00425  CALL-STORAGE-MANAGER.                                            ELUMEMBR
00426      EXEC CICS LINK                                               ELUMEMBR
00427                PROGRAM ('ELUSTGMG')                               ELUMEMBR
00428                COMMAREA(DFHCOMMAREA)                              ELUMEMBR
00429                END-EXEC.                                          ELUMEMBR
00430                                                                   ELUMEMBR
00431                                                                   ELUMEMBR
00432 ************************************************************      ELUMEMBR
00433 *                                                          *      ELUMEMBR
00434 *        GETMAIN CSEXECIO-CONTROL                          *      ELUMEMBR
00435 *                                                          *      ELUMEMBR
00436 ************************************************************      ELUMEMBR
00437  GETMAIN-CSEXECIO-CONTROL.                                        ELUMEMBR
00438      SET CIA-COBXIO-DDN              TO TRUE.                     ELUMEMBR
00439      SET CIA-STG-GETMAIN             TO TRUE.                     ELUMEMBR
00440      PERFORM CALL-STORAGE-MANAGER.                                ELUMEMBR
00441      SET CIA-COBXIO-DDN TO TRUE.                                  ELUMEMBR
00442      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUMEMBR
00443          ADDRESS OF CSEXECIO-CONTROL.                             ELUMEMBR
00444                                                                   ELUMEMBR
00445                                                                   ELUMEMBR
00446 ************************************************************      ELUMEMBR
00447 *                                                          *      ELUMEMBR
00448 *        STOW MEMBERSHIP INFORMATION                       *      ELUMEMBR
00449 *                                                          *      ELUMEMBR
00450 ************************************************************      ELUMEMBR
00451  STOW-MEMBERSHIP-INFORMATION.                                     ELUMEMBR
00452      SET CIA-ELSMEMS-DDN                     TO TRUE.             ELUMEMBR
00453      MOVE LENGTH OF MSI-MEMBERSHIP-INTERFACE TO CIA-AREA-LEN.     ELUMEMBR
00454      SET CIA-STG-STOW                        TO TRUE.             ELUMEMBR
00455      PERFORM CALL-STORAGE-MANAGER.                                ELUMEMBR
00456                                                                   ELUMEMBR
00457                                                                   ELUMEMBR
00458 ************************************************************      ELUMEMBR
00459 *                                                          *      ELUMEMBR
00460 *        PROCESS                                           *      ELUMEMBR
00461 *                                                          *      ELUMEMBR
00462 ************************************************************      ELUMEMBR
00463  PROCESS.                                                         ELUMEMBR
00464 ******************************************************************ELUMEMBR
00465 *  BROWSE THE MEMBERSHIP FILE USING THE GROUP AND MEMBER NUMBERS  ELUMEMBR
00466 *  PASSED TO US IN THE SSCB BLK TO FIND THE MOST RECENT MEMBERSHIPELUMEMBR
00467 *  RECORD.  THIS BROWSE IS USED TO DETERMINE THAT THERE EXISTS    ELUMEMBR
00468 *  AT LEAST ONE MEMBERSHIP RECORD OUT ON THE FILE FOR THE         ELUMEMBR
00469 *  GROUP/MEMBER NUMBERS PASSED TO US.           REB ==> 05/26/88  ELUMEMBR
00470 ******************************************************************ELUMEMBR
00471      PERFORM TEXAS-REGION-CHECK.                                  ELUMEMBR
00472 *    PERFORM PKG-DATE-CONVERSION.                                 ELUMEMBR
00473      MOVE ZERO TO MSI-GRP-SUB-EFF-DATE-CC                         ELUMEMBR
00474                   MSI-GRP-SUB-TERM-DATE-CENTURY.                  ELUMEMBR
00475      MOVE ZEROES TO WS-LAST-EFF-DATE-CEN                          ELUMEMBR
00476                     WS-LAST-CAN-DATE-CEN.                         ELUMEMBR
00477      INITIALIZE WS-MEMB-TABLE-AREA.                               ELUMEMBR
00478      PERFORM VARYING WS-MBR-IDX FROM 1 BY 1                       ELUMEMBR
00479          UNTIL WS-MBR-IDX > 20                                    ELUMEMBR
00480            MOVE ZEROES TO WS-MBR-EFF-DATE-CEN (WS-MBR-IDX)        ELUMEMBR
00481            MOVE ZEROES TO WS-MBR-CAN-DATE-CEN (WS-MBR-IDX)        ELUMEMBR
00482      END-PERFORM.                                                 ELUMEMBR
00483      PERFORM VARYING MSI-IDX FROM 1 BY 1                          ELUMEMBR
00484          UNTIL MSI-IDX > 20                                       ELUMEMBR
00485            MOVE ZEROES TO MSI-EFF-DATE-CENTURY (MSI-IDX)          ELUMEMBR
00486            MOVE ZEROES TO MSI-TERMIN-DATE-CC   (MSI-IDX)          ELUMEMBR
00487      END-PERFORM.                                                 ELUMEMBR
00488      SET MSI-IDX TO 1.                                            ELUMEMBR
00489      PERFORM BROWSE-THE-MEMBERSHIP-FILE                           ELUMEMBR
00490          VARYING WS-MEMB-BROWSE FROM WS-MEMB-BROWSE BY +1         ELUMEMBR
00491                  UNTIL   WS-MEMB-BROWSE > +158                    ELUMEMBR
00492                  OR      STOP-MEMB-BROWSE.                        ELUMEMBR
00493      IF WS-MEMB-BROWSE > +158                                     ELUMEMBR
00494          PERFORM SIGNAL-IO-ERROR                                  ELUMEMBR
00495      ELSE                                                         ELUMEMBR
00496         IF MSI-NBR-MBR-SECTNS  = 1                   AND          ELUMEMBR
00497 *               MSI-PREV-GROUP-NUMBER    = SPACES                 ELUMEMBR
00498 *        AND                                                      ELUMEMBR
00499                 MSI-PREV-MEMBR-NBR  = SPACES                      ELUMEMBR
00500          AND                                                      ELUMEMBR
00501                 MSI-NEXT-GROUP-NUMBER    = SPACES                 ELUMEMBR
00502          AND                                                      ELUMEMBR
00503                 MSI-NEXT-MEMBR-NBR  = SPACES                      ELUMEMBR
00504          AND                                                      ELUMEMBR
00505                 MSI-GRP-SUB-EFF-DATE-CC  = ZEROS                  ELUMEMBR
00506          AND                                                      ELUMEMBR
00507                 MSI-GRP-SUB-TERM-DATE-CENTURY = ZEROS             ELUMEMBR
00508          AND                                                      ELUMEMBR
00509                (SSB-SRV-FROM-DT-CEN NOT                           ELUMEMBR
00510                          < MSI-EFF-DATE-CENTURY  (1))             ELUMEMBR
00511          AND                                                      ELUMEMBR
00512                (SSB-SRV-TO-DT-CEN   NOT > MSI-TERMIN-DATE-CC      ELUMEMBR
00513          (1))                                                     ELUMEMBR
00514            PERFORM INSERT-THE-ONLY-SECTION-IN-SSC                 ELUMEMBR
00515         ELSE                                                      ELUMEMBR
00516            IF CIA-RETURN-CODE = ZERO               AND            ELUMEMBR
00517                (MSI-NBR-MBR-SECTNS  > ZERO            OR          ELUMEMBR
00518                 MSI-PREV-GROUP-NUMBER    NOT = SPACES      OR     ELUMEMBR
00519                 MSI-PREV-MEMBR-NBR  NOT = SPACES      OR          ELUMEMBR
00520                 MSI-NEXT-GROUP-NUMBER    NOT = SPACES      OR     ELUMEMBR
00521                 MSI-NEXT-MEMBR-NBR  NOT = SPACES      OR          ELUMEMBR
00522                 MSI-GRP-SUB-EFF-DATE-CC      > ZERO         OR    ELUMEMBR
00523                 MSI-GRP-SUB-TERM-DATE-CENTURY         > ZERO)     ELUMEMBR
00524 *- TEMPORARY PKG-CODE STUFF                                       ELUMEMBR
00525               PERFORM TEXAS-REGION-CHECK                          ELUMEMBR
00526 *             IF TEXAS-REGION                                     ELUMEMBR
00527 *                MOVE SSB-GRP-NO TO PSKP-GROUP                    ELUMEMBR
00528 *                IF NOT SKIP-IT                                   ELUMEMBR
00529 *                   PERFORM  PKG-CODE-GROUP                       ELUMEMBR
00530 *                           VARYING WS-PKG-SUB1 FROM 1 BY 1       ELUMEMBR
00531 *                     UNTIL WS-PKG-SUB1 > MSI-NBR-MBR-SECTNS      ELUMEMBR
00532 *                END-IF                                           ELUMEMBR
00533 *             END-IF                                              ELUMEMBR
00534 *             IF NOT TEXAS-REGION                                 ELUMEMBR
00535 *               MOVE 000 TO SSB-PKG-CODE                          ELUMEMBR
00536 *               PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL          ELUMEMBR
00537 *                  WS-SUB > MSI-NBR-MBR-SECTNS                    ELUMEMBR
00538 *                    MOVE 000 TO MSI-PKG-CODE (WS-SUB)            ELUMEMBR
00539 *               END-PERFORM                                       ELUMEMBR
00540 *             END-IF                                              ELUMEMBR
00541 *-/                                                               ELUMEMBR
00542            END-IF                                                 ELUMEMBR
00543            PERFORM STOW-MEMBERSHIP-INFORMATION.                   ELUMEMBR
00544                                                                   ELUMEMBR
00545                                                                   ELUMEMBR
00546 ************************************************************      ELUMEMBR
00547 *                                                          *      ELUMEMBR
00548 *        INSERT THE ONLY SECTION IN SSCB AREA              *      ELUMEMBR
00549 *                                                          *      ELUMEMBR
00550 ************************************************************      ELUMEMBR
00551  INSERT-THE-ONLY-SECTION-IN-SSC.                                  ELUMEMBR
00552 *******************************************                       ELUMEMBR
00553 * THIS WILL CAUSE AN AUTOMATIC SELECT !   *                       ELUMEMBR
00554 *******************************************                       ELUMEMBR
00555      IF (NOT TEXAS-REGION) OR WS-CHANGE-TX-PKG-CODE-GROUP         ELUMEMBR
00556        MOVE MSI-MEMBER-SECTION (1)  TO SSB-SECTN-NO               ELUMEMBR
00557        MOVE 000  TO SSB-PKG-CODE                                  ELUMEMBR
00558      ELSE                                                         ELUMEMBR
00559          MOVE MSI-MEMBER-SECTION (1)  TO SSB-SECTN-NO             ELUMEMBR
00560          MOVE MSI-PKG-CODE (1) TO SSB-PKG-CODE                    ELUMEMBR
00561      END-IF.                                                      ELUMEMBR
00562                                                                   ELUMEMBR
00563 *-/                                                               ELUMEMBR
00564                                                                   ELUMEMBR
00565                                                                   ELUMEMBR
00566 ************************************************************      ELUMEMBR
00567 *                                                          *      ELUMEMBR
00568 *        READ GROUP MASTER RECORD                          *      ELUMEMBR
00569 *                                                          *      ELUMEMBR
00570 ************************************************************      ELUMEMBR
00571  READ-GROUP-MASTER-RECORD.                                        ELUMEMBR
00572 ******************************************************************ELUMEMBR
00573 * READ THE GROUP MASTER FILE TO DETERMINE THE TYPE OF BUSINESS    ELUMEMBR
00574 * (RETENTION OR NON-RETENTION) AND SET THE INDICATORS IN THE      ELUMEMBR
00575 * COMMAREA RESPECTIVELY BEFORE RETURNING TO CICS.                 ELUMEMBR
00576 *                                              REB ==> 05/26/88   ELUMEMBR
00577 ******************************************************************ELUMEMBR
00578      SET XIO-GMF-FIND        TO TRUE.                             ELUMEMBR
00579      MOVE LOW-VALUES         TO XIOSCT.                           ELUMEMBR
00580      MOVE SSB-GRP-NO         TO XIOGRP.                           ELUMEMBR
00581      PERFORM SETUP-IO-PARMS-AND-LINK-TO-MEM.                      ELUMEMBR
00582      IF XIO-OK                                                    ELUMEMBR
00583          PERFORM PROCESS-GROUP-MASTER-RECORD-FO                   ELUMEMBR
00584      ELSE                                                         ELUMEMBR
00585         SET CIA-RC-MEMB-GRP-NOTFND TO TRUE.                       ELUMEMBR
00586                                                                   ELUMEMBR
00587                                                                   ELUMEMBR
00588 ************************************************************      ELUMEMBR
00589 *                                                          *      ELUMEMBR
00590 *        PROCESS GROUP MASTER RECORD FOUND                 *      ELUMEMBR
00591 *                                                          *      ELUMEMBR
00592 ************************************************************      ELUMEMBR
00593  PROCESS-GROUP-MASTER-RECORD-FO.                                  ELUMEMBR
00594      MOVE 0 TO MSI-NBR-MBR-SECTNS.                                ELUMEMBR
00595      SET ADDRESS OF GROUP-HEADER TO XIO-PTR (1).                  ELUMEMBR
00596      IF 000-TYPE-OF-BUSINESS = '11'  OR  '31'  OR  '32' OR        ELUMEMBR
00597          '43'                                                     ELUMEMBR
00598         SET CIA-RC-MEMB-NON-RETN   TO TRUE                        ELUMEMBR
00599      ELSE                                                         ELUMEMBR
00600         SET CIA-RC-MEMB-NOT-IN-GRP TO TRUE.                       ELUMEMBR
00601                                                                   ELUMEMBR
00602                                                                   ELUMEMBR
00603 ************************************************************      ELUMEMBR
00604 *                                                          *      ELUMEMBR
00605 *        BROWSE THE MEMBERSHIP FILE                        *      ELUMEMBR
00606 *                                                          *      ELUMEMBR
00607 ************************************************************      ELUMEMBR
00608  BROWSE-THE-MEMBERSHIP-FILE.                                      ELUMEMBR
00609 ******************************************************************ELUMEMBR
00610 * THIS ROUTINE PERFORMS THE INITIAL BROWSE OF THE MEMBERSHIP      ELUMEMBR
00611 * FILE TO DETERMINE IF THERE ARE ANY MEMBERSHIP RECORDS           ELUMEMBR
00612 * FOR THE GROUP/SUBSCRIBER NUMBER PASSED TO US IN THE SSCB BLOCK. ELUMEMBR
00613 * THE FIELD REPRESENTS THE READ REQUEST PARAMETER FOR MEMBERSHIP  ELUMEMBR
00614 * FILE. THERE IS A MAXIMUM LIMIT OF 9 SECTIONS THAT CAN BE READ   ELUMEMBR
00615 * FOR A GROUP/SUBSCRIBER. (ACCEPTABLE VALUES ARE FROM 150 TO 158) ELUMEMBR
00616 * THIS WILL READ THE GROUP MASTER RECORD IF NO \
00617 * MEMBER RECORD.                            REB ==> 05/26/88      ELUMEMBR
00618 *                                                                 ELUMEMBR
00619 ******************************************************************ELUMEMBR
00620      MOVE WS-MEMB-BROWSE     TO XIORQ.                            ELUMEMBR
00621      MOVE SSB-GRP-NO         TO XIOGRP.                           ELUMEMBR
00622      MOVE LOW-VALUES         TO XIOSCT.                           ELUMEMBR
00623      MOVE SSB-SUBSCRIBER-NBR TO XIOSUB.                           ELUMEMBR
00624      PERFORM SETUP-IO-PARMS-AND-LINK-TO-SMF.                      ELUMEMBR
00625      IF XIO-NO-TRLR-MORE-SECT                                     ELUMEMBR
00626          CONTINUE                                                 ELUMEMBR
00627      ELSE                                                         ELUMEMBR
00628          PERFORM DETERMINE-RESULTS-OF-MEMBERSHI.                  ELUMEMBR
00629                                                                   ELUMEMBR
00630                                                                   ELUMEMBR
00631 ************************************************************      ELUMEMBR
00632 *                                                          *      ELUMEMBR
00633 *        SETUP IO PARMS AND LINK TO SMF MODULE             *      ELUMEMBR
00634 *                                                          *      ELUMEMBR
00635 ************************************************************      ELUMEMBR
00636  SETUP-IO-PARMS-AND-LINK-TO-SMF.                                  ELUMEMBR
00637 *    MOVE '1'                TO XIOPARAM.                         ELUMEMBR
00638      MOVE '1000200000000000000000000000000000000000'              ELUMEMBR
00639          TO XIOPARAM.                                             ELUMEMBR
00640      SET XIO-PTR-MODE        TO TRUE.                             ELUMEMBR
00641      SET XIO-OK              TO TRUE.                             ELUMEMBR
00642      PERFORM CALL-CSEXECIO-INTERFACE.                             ELUMEMBR
00643                                                                   ELUMEMBR
00644                                                                   ELUMEMBR
00645                                                                   ELUMEMBR
00646 ************************************************************      ELUMEMBR
00647 *                                                          *      ELUMEMBR
00648 *        SETUP IO PARMS AND LINK TO MEMBERSHIP MODULE      *      ELUMEMBR
00649 *                                                          *      ELUMEMBR
00650 ************************************************************      ELUMEMBR
00651  SETUP-IO-PARMS-AND-LINK-TO-MEM.                                  ELUMEMBR
00652      MOVE '1'                TO XIOPARAM.                         ELUMEMBR
00653      SET XIO-PTR-MODE        TO TRUE.                             ELUMEMBR
00654      SET XIO-OK              TO TRUE.                             ELUMEMBR
00655      PERFORM CALL-CSEXECIO-INTERFACE.                             ELUMEMBR
00656                                                                   ELUMEMBR
00657                                                                   ELUMEMBR
00658 ************************************************************      ELUMEMBR
00659 *                                                          *      ELUMEMBR
00660 *        DETERMINE RESULTS OF MEMBERSHIP BROWSE            *      ELUMEMBR
00661 *                                                          *      ELUMEMBR
00662 ************************************************************      ELUMEMBR
00663  DETERMINE-RESULTS-OF-MEMBERSHI.                                  ELUMEMBR
00664      IF XIO-NOT-FOUND                                             ELUMEMBR
00665          PERFORM READ-GROUP-MASTER-RECORD                         ELUMEMBR
00666      ELSE                                                         ELUMEMBR
00667          SET ADDRESS OF MEMBER-HEADER TO XIO-PTR (1)              ELUMEMBR
00668          MOVE 470-SECTION-NUMBER TO WS-HOLD-SECT-NO-4             ELUMEMBR
00669          IF TEXAS-REGION                                          ELUMEMBR
00670             IF WS-CHANGE-TX-PKG-CODE-GROUP                        ELUMEMBR
00671                MOVE ZEROES TO SSB-PKG-CODE                        ELUMEMBR
00672             ELSE                                                  ELUMEMBR
00673                PERFORM READ-TX-SUPPLEMENTAL                       ELUMEMBR
00674             END-IF                                                ELUMEMBR
00675          END-IF                                                   ELUMEMBR
00676          PERFORM SAVE-MEMBERSHIP-INFO-AND-QUALI.                  ELUMEMBR
00677      SET STOP-MEMB-BROWSE         TO TRUE.                        ELUMEMBR
00678                                                                   ELUMEMBR
00679                                                                   ELUMEMBR
00680 ************************************************************      ELUMEMBR
00681 *                                                          *      ELUMEMBR
00682 *        SAVE MEMBERSHIP INFO AND QUALIFY THE FIELDS       *      ELUMEMBR
00683 *                                                          *      ELUMEMBR
00684 ************************************************************      ELUMEMBR
00685  SAVE-MEMBERSHIP-INFO-AND-QUALI.                                  ELUMEMBR
00686      PERFORM INTERROGATE-THE-FIRST-GOOD-MEM.                      ELUMEMBR
00687      PERFORM PROCESS-ANY-CHAINING-THAT-EXIS.                      ELUMEMBR
00688                                                                   ELUMEMBR
00689                                                                   ELUMEMBR
00690 ************************************************************      ELUMEMBR
00691 *                                                          *      ELUMEMBR
00692 *        INTERROGATE THE FIRST GOOD MEMBER RECORD          *      ELUMEMBR
00693 *                                                          *      ELUMEMBR
00694 ************************************************************      ELUMEMBR
00695  INTERROGATE-THE-FIRST-GOOD-MEM.                                  ELUMEMBR
00696      SET ADDRESS OF MEMBER-HEADER TO XIO-PTR (1).                 ELUMEMBR
00697      INITIALIZE                      MSI-MBR-INFO.                ELUMEMBR
00698      MOVE ZERO TO MSI-GRP-SUB-EFF-DATE-CC                         ELUMEMBR
00699                   MSI-GRP-SUB-TERM-DATE-CENTURY.                  ELUMEMBR
00700      MOVE 470-SUBS-FIRST-NAME     TO MSI-FIRST-NAME.              ELUMEMBR
00701      MOVE 470-SUBS-LAST-NAME      TO MSI-LAST-NAME.               ELUMEMBR
00702      MOVE 470-SUBS-MID-INIT       TO                              ELUMEMBR
00703          MSI-MIDDLE-INITIAL.                                      ELUMEMBR
00704      MOVE 470-TRANSFER-TO-NO      TO WS-HOLD-TRANSFER-KEY.        ELUMEMBR
00705      PERFORM CONVERT-BOTH-EFFECTIVE-AND-CAN.                      ELUMEMBR
00706      PERFORM CHECK-THAT-MEMBERSHIP-RECORD-F.                      ELUMEMBR
00707      IF (NOT TEXAS-REGION) OR WS-CHANGE-TX-PKG-CODE-GROUP         ELUMEMBR
00708         ADD +1                   TO WS-SECTION-ENTRIES            ELUMEMBR
00709         MOVE ZEROES TO WS-SECTION-NUMBER (WS-SECTION-ENTRIES)     ELUMEMBR
00710         MOVE 470-SECTION-NUMBER  TO WS-SECTION-NBR                ELUMEMBR
00711              (WS-SECTION-ENTRIES)                                 ELUMEMBR
00712         MOVE 000 TO WS-PKG-CODE (WS-SECTION-ENTRIES)              ELUMEMBR
00713      END-IF.                                                      ELUMEMBR
00714                                                                   ELUMEMBR
00715                                                                   ELUMEMBR
00716 ************************************************************      ELUMEMBR
00717 *                                                          *      ELUMEMBR
00718 *        CONVERT BOTH EFFECTIVE AND CANCEL DATES TO JULIAN *      ELUMEMBR
00719 *                                                          *      ELUMEMBR
00720 ************************************************************      ELUMEMBR
00721  CONVERT-BOTH-EFFECTIVE-AND-CAN.                                  ELUMEMBR
00722      IF 470-BC-BASIC-ORIG-EFF-DT <                                ELUMEMBR
00723          470-BS-BASIC-ORIG-EFF-DT                                 ELUMEMBR
00724          MOVE 470-BC-BASIC-ORIG-EFF-DT   TO DATE-YMD              ELUMEMBR
00725      ELSE                                                         ELUMEMBR
00726          MOVE 470-BS-BASIC-ORIG-EFF-DT   TO DATE-YMD              ELUMEMBR
00727      END-IF.                                                      ELUMEMBR
00728      IF 470-BC-BASIC-ORIG-EFF-DT = LOW-VALUES                     ELUMEMBR
00729        AND                                                        ELUMEMBR
00730          470-BS-BASIC-ORIG-EFF-DT = LOW-VALUES                    ELUMEMBR
00731         MOVE 484-ORIG-EFF-DATE(1) TO DATE-YMD                     ELUMEMBR
00732      END-IF.                                                      ELUMEMBR
00733      MOVE '2' TO WS-CSDTCONV-OPERATION.                           ELUMEMBR
00734      PERFORM CONVERT-THE-DATE-GIVEN-TO-JULI.                      ELUMEMBR
00735 *                                                                 ELUMEMBR
00736 *    TAKE DATE FROM CSDTCONV AND MAKE IT JULIAN VIA MLDATES       ELUMEMBR
00737 *                                                                 ELUMEMBR
00738      PERFORM LINK-MLDATES.                                        ELUMEMBR
00739      MOVE MLDATE-JUL2   TO WS-HOLD-EFF-DT-CEN.                    ELUMEMBR
00740                                                                   ELUMEMBR
00741      MOVE 470-CANCEL-DATE   TO DATE-YMD.                          ELUMEMBR
00742      MOVE '2' TO WS-CSDTCONV-OPERATION.                           ELUMEMBR
00743      PERFORM CONVERT-THE-DATE-GIVEN-TO-JULI.                      ELUMEMBR
00744      PERFORM LINK-MLDATES.                                        ELUMEMBR
00745 *                                                                 ELUMEMBR
00746 *    TAKE DATE FROM CSDTCONV AND MAKE IT JULIAN VIA MLDATES       ELUMEMBR
00747 *                                                                 ELUMEMBR
00748      MOVE MLDATE-JUL2   TO WS-HOLD-CAN-DT-CEN.                    ELUMEMBR
00749                                                                   ELUMEMBR
00750 ************************************************************      ELUMEMBR
00751 *                                                          *      ELUMEMBR
00752 *        LINK MLDATES (GIVES CENTURY JULIAN AFTER CALL     *      ELUMEMBR
00753 *       TO CSDTCONV)                                       *      ELUMEMBR
00754 ************************************************************      ELUMEMBR
00755  LINK-MLDATES.                                                    ELUMEMBR
00756      MOVE  'CNV' TO MLDATE-FUNC.                                  ELUMEMBR
00757      MOVE 'Y' TO MLDATE-FORM1.                                    ELUMEMBR
00758      MOVE 'J' TO MLDATE-FORM2.                                    ELUMEMBR
00759                                                                   ELUMEMBR
00760      EVALUATE TRUE                                                ELUMEMBR
00761         WHEN WS-485-CONVERT                                       ELUMEMBR
00762            IF WS-485-HOLD-CC-YY  = LOW-VALUES                     ELUMEMBR
00763               MOVE 99991231 TO MLDATE-DATE1                       ELUMEMBR
00764            ELSE                                                   ELUMEMBR
00765               MOVE WS-485-HOLD-CC-YY TO MLDATE-DATE1              ELUMEMBR
00766            END-IF                                                 ELUMEMBR
00767         WHEN OTHER                                                ELUMEMBR
00768            IF  DATE-YMD = LOW-VALUES                              ELUMEMBR
00769               MOVE 99991231 TO MLDATE-DATE1                       ELUMEMBR
00770            ELSE                                                   ELUMEMBR
00771               MOVE WS-CSDTCONV-UNPACKED-DT  TO MLDATE-DATE1       ELUMEMBR
00772            END-IF                                                 ELUMEMBR
00773      END-EVALUATE.                                                ELUMEMBR
00774                                                                   ELUMEMBR
00775     EXEC CICS LINK PROGRAM ('MLDATEC')                            ELUMEMBR
00776                    COMMAREA (MLDATE01)                            ELUMEMBR
00777                      LENGTH (28)                                  ELUMEMBR
00778     END-EXEC.                                                     ELUMEMBR
00779                                                                   ELUMEMBR
00780      IF MLDATE-RETURN > 00                                        ELUMEMBR
00781          MOVE ZEROES TO MLDATE-JUL2                               ELUMEMBR
00782      END-IF.                                                      ELUMEMBR
00783                                                                   ELUMEMBR
00784 ************************************************************      ELUMEMBR
00785 *                                                          *      ELUMEMBR
00786 *        CONVERT THE DATE GIVEN TO JULIAN FORM             *      ELUMEMBR
00787 *                                                          *      ELUMEMBR
00788 ************************************************************      ELUMEMBR
00789  CONVERT-THE-DATE-GIVEN-TO-JULI.                                  ELUMEMBR
00790                                                                   ELUMEMBR
00791      MOVE 'CSDTCONV' TO WS-ACTION-MODULE.                         ELUMEMBR
00792      CALL WS-ACTION-MODULE  USING DATE-YMD                        ELUMEMBR
00793                           WS-CSDTCONV-UNPACKED-DT                 ELUMEMBR
00794                           WS-CSDTCONV-OPERATION.                  ELUMEMBR
00795 *    IF DATE-YMD = LOW-VALUES                                     ELUMEMBR
00796 *       MOVE 19991231           TO WS-CSDTCONV-UNPACKED-DT-6.     ELUMEMBR
00797 *    ELSE                                                         ELUMEMBR
00798 *        MOVE WS-DATE-FIELD      TO WS-CSDTCONV-UNPACKED-DT-6     ELUMEMBR
00799 *    END-IF.                                                      ELUMEMBR
00800 *    IF CSDTCNV-RETURN > ZEROES                                   ELUMEMBR
00801 *        MOVE ZEROS           TO WS-CSDTCONV-UNPACKED-DT          ELUMEMBR
00802 *    END-IF.                                                      ELUMEMBR
00803                                                                   ELUMEMBR
00804                                                                   ELUMEMBR
00805 ************************************************************      ELUMEMBR
00806 *                                                          *      ELUMEMBR
00807 *        CALL HGADATES INTERFACE                           *      ELUMEMBR
00808 *                                                          *      ELUMEMBR
00809 ************************************************************      ELUMEMBR
00810  CALL-HGADATES-INTERFACE.                                         ELUMEMBR
00811      EXEC CICS LINK                                               ELUMEMBR
00812                PROGRAM('HGADATES')                                ELUMEMBR
00813                COMMAREA(HGADATES-PARM-LIST)                       ELUMEMBR
00814                END-EXEC.                                          ELUMEMBR
00815                                                                   ELUMEMBR
00816                                                                   ELUMEMBR
00817 ************************************************************      ELUMEMBR
00818 *                                                          *      ELUMEMBR
00819 *        CHECK THAT MEMBERSHIP RECORD FALLS IN DATE RANGE G*      ELUMEMBR
00820 *                                                          *      ELUMEMBR
00821 ************************************************************      ELUMEMBR
00822  CHECK-THAT-MEMBERSHIP-RECORD-F.                                  ELUMEMBR
00823      IF WS-HOLD-EFF-DT-CEN < SSB-SRV-FROM-DT-CEN      AND         ELUMEMBR
00824                 WS-HOLD-CAN-DT-CEN < SSB-SRV-FROM-DT-CEN          ELUMEMBR
00825          PERFORM RETAIN-INFO-FROM-LAST-RECORD                     ELUMEMBR
00826      ELSE IF WS-HOLD-EFF-DT-CEN > SSB-SRV-TO-DT-CEN    OR         ELUMEMBR
00827                 WS-HOLD-CAN-DT-CEN < SSB-SRV-FROM-DT-CEN          ELUMEMBR
00828          OR                                                       ELUMEMBR
00829                 WS-HOLD-EFF-DT-CEN = ZEROS                        ELUMEMBR
00830          OR                                                       ELUMEMBR
00831                 WS-HOLD-CAN-DT-CEN = ZEROS                        ELUMEMBR
00832          PERFORM MOVE-EFFECTIVE-AND-CANCEL-DATE                   ELUMEMBR
00833      ELSE                                                         ELUMEMBR
00834          PERFORM DETERMINE-ADD-METHOD.                            ELUMEMBR
00835                                                                   ELUMEMBR
00836  DETERMINE-ADD-METHOD.                                            ELUMEMBR
00837      EVALUATE TRUE                                                ELUMEMBR
00838      WHEN NOT TEXAS-REGION                                        ELUMEMBR
00839           PERFORM ADD-UNIQUE-SECTION-NUMBER-TO-W                  ELUMEMBR
00840      WHEN TEXAS-REGION AND (WS-MBR-ENTRY-CNT = 1) AND             ELUMEMBR
00841                 4805-MEM-NO-OF-HISTORY > 1                        ELUMEMBR
00842         MOVE WS-MBR-SCT-NBR    (MSI-IDX) TO MSI-MEMBER-SECTION    ELUMEMBR
00843             (MSI-IDX)                                             ELUMEMBR
00844         MOVE WS-MBR-PKG-CODE (MSI-IDX) TO MSI-PKG-CODE (MSI-IDX)  ELUMEMBR
00845         MOVE WS-MBR-EFF-DATE-CEN (MSI-IDX)                        ELUMEMBR
00846                      TO MSI-EFF-DATE-CENTURY (MSI-IDX)            ELUMEMBR
00847         MOVE WS-MBR-CAN-DATE-CEN (MSI-IDX)                        ELUMEMBR
00848                        TO MSI-TERMIN-DATE-CC (MSI-IDX)            ELUMEMBR
00849      WHEN OTHER                                                   ELUMEMBR
00850         PERFORM LOAD-MEMBERSHIP-INTERFACE-TABL                    ELUMEMBR
00851             VARYING MSI-IDX FROM 1 BY 1                           ELUMEMBR
00852                     UNTIL   MSI-IDX > 20                          ELUMEMBR
00853                     OR      MSI-IDX > WS-MBR-ENTRY-CNT            ELUMEMBR
00854 *          IF TEXAS-REGION AND (WS-MEM-PKG-FND-SW = 'Y')          ELUMEMBR
00855 *            COMPUTE MSI-NBR-MBR-SECTNS = MSI-NBR-MBR-SECTNS - 1  ELUMEMBR
00856 *          END-IF                                                 ELUMEMBR
00857      END-EVALUATE.                                                ELUMEMBR
00858                                                                   ELUMEMBR
00859 ************************************************************      ELUMEMBR
00860 *                                                          *      ELUMEMBR
00861 *        ADD UNIQUE SECTION NUMBER TO WS MEMB TABLE        *      ELUMEMBR
00862 *                                                          *      ELUMEMBR
00863 ************************************************************      ELUMEMBR
00864  ADD-UNIQUE-SECTION-NUMBER-TO-W.                                  ELUMEMBR
00865      ADD  +1                    TO WS-MBR-ENTRY-CNT.              ELUMEMBR
00866      MOVE ZEROES TO WS-MBR-SCT-NBR (WS-MBR-ENTRY-CNT)             ELUMEMBR
00867                     WS-PKG-CODE (WS-MBR-ENTRY-CNT).               ELUMEMBR
00868      MOVE 470-SECTION-NUMBER    TO WS-MBR-SCT-NBR-4               ELUMEMBR
00869          (WS-MBR-ENTRY-CNT).                                      ELUMEMBR
00870      MOVE WS-HOLD-EFF-DT-CEN    TO WS-MBR-EFF-DATE-CEN            ELUMEMBR
00871          (WS-MBR-ENTRY-CNT).                                      ELUMEMBR
00872      MOVE WS-HOLD-CAN-DT-CEN    TO WS-MBR-CAN-DATE-CEN            ELUMEMBR
00873          (WS-MBR-ENTRY-CNT).                                      ELUMEMBR
00874      IF (NOT TEXAS-REGION) OR WS-CHANGE-TX-PKG-CODE-GROUP         ELUMEMBR
00875         MOVE ZEROES TO WS-MBR-PKG-CODE (WS-MBR-ENTRY-CNT)         ELUMEMBR
00876 *    ELSE                                                         ELUMEMBR
00877 *       PERFORM TEXAS-PKG-CODE-CHECK                              ELUMEMBR
00878      END-IF.                                                      ELUMEMBR
00879                                                                   ELUMEMBR
00880 ************************************************************      ELUMEMBR
00881 *                                                          *      ELUMEMBR
00882 *        TEXAS PACKAGE CODE CHECK                          *      ELUMEMBR
00883 *                                                          *      ELUMEMBR
00884 ************************************************************      ELUMEMBR
00885  TEXAS-PKG-CODE-CHECK.                                            ELUMEMBR
00886      SET 4805-MEM-HIST-IDX TO 1.                                  ELUMEMBR
00887      IF 4805-MEM-NO-OF-HISTORY > 1                                ELUMEMBR
00888 *      AND WS-MEM-PKG-FND-SW = 'N'                                ELUMEMBR
00889         PERFORM CHECK-FOR-ADDN-PKG-CODE                           ELUMEMBR
00890      ELSE                                                         ELUMEMBR
00891       ADD +1 TO WS-MBR-ENTRY-CNT                                  ELUMEMBR
00892          SET 4805-MEM-HIST-IDX TO 1                               ELUMEMBR
00893          MOVE 0 TO WS-MBR-SCT-NBR-1 (WS-MBR-ENTRY-CNT)            ELUMEMBR
00894          MOVE 470-SECTION-NUMBER TO                               ELUMEMBR
00895                    WS-MBR-SCT-NBR-4 (WS-MBR-ENTRY-CNT)            ELUMEMBR
00896                    SSB-SECT-NO                                    ELUMEMBR
00897          MOVE 4805-PACKAGE-CODE (4805-MEM-HIST-IDX)               ELUMEMBR
00898                TO WS-MBR-PKG-CODE (WS-MBR-ENTRY-CNT)              ELUMEMBR
00899                   SSB-PKG-CODE                                    ELUMEMBR
00900      END-IF.                                                      ELUMEMBR
00901                                                                   ELUMEMBR
00902                                                                   ELUMEMBR
00903 ************************************************************      ELUMEMBR
00904 *                                                          *      ELUMEMBR
00905 *        CHECK FOR ADDN PKG CODES                          *      ELUMEMBR
00906 * IF NO PKG CODE FOUND WILL MOVE ZEROES AND SMF EFF AND    *      ELUMEMBR
00907 * TRM DATES TO LET ELIQ CONTINUE.  THIS SHOULD CAUSE A     *      ELUMEMBR
00908 * 'NOT FOUND' DOWN THE LINE.                               *      ELUMEMBR
00909 ************************************************************      ELUMEMBR
00910  CHECK-FOR-ADDN-PKG-CODE.                                         ELUMEMBR
00911      PERFORM 4805-CREATE-CENTURY.                                 ELUMEMBR
00912      IF  (SSB-SRV-FROM-DT-CEN   NOT <                             ELUMEMBR
00913             WS-485-JUL-EFF)                                       ELUMEMBR
00914         AND   (SSB-SRV-TO-DT-CEN    NOT >                         ELUMEMBR
00915            WS-485-JUL-END)                                        ELUMEMBR
00916            OR (4805-MEM-END-DATE-YMD (4805-MEM-HIST-IDX) = 0)     ELUMEMBR
00917               PERFORM MOVE-FIRST-PKG-CODE                         ELUMEMBR
00918      END-IF.                                                      ELUMEMBR
00919      SET 4805-MEM-HIST-IDX UP BY 1.                               ELUMEMBR
00920      PERFORM VARYING 4805-MEM-HIST-IDX FROM 4805-MEM-HIST-IDX     ELUMEMBR
00921           BY 1 UNTIL 4805-MEM-HIST-IDX > 4805-MEM-NO-OF-HISTORY   ELUMEMBR
00922          IF 4805-PACKAGE-CODE (4805-MEM-HIST-IDX - 1) =           ELUMEMBR
00923             4805-PACKAGE-CODE (4805-MEM-HIST-IDX)                 ELUMEMBR
00924               PERFORM 4805-CREATE-CENTURY                         ELUMEMBR
00925               MOVE WS-485-JUL-EFF TO WS-MBR-EFF-DATE-CEN          ELUMEMBR
00926                     (WS-MBR-ENTRY-CNT - 1)                        ELUMEMBR
00927          ELSE                                                     ELUMEMBR
00928             PERFORM SEARCH-FOR-MULT-PKG-DATES                     ELUMEMBR
00929 *           PERFORM HANDLE-PKG-CODE-FIND                          ELUMEMBR
00930          END-IF                                                   ELUMEMBR
00931 *        ADD 1 TO WS-MBR-ENTRY-CNT                                ELUMEMBR
00932      END-PERFORM.                                                 ELUMEMBR
00933                                                                   ELUMEMBR
00934 ************************************************************      ELUMEMBR
00935 *                                                          *      ELUMEMBR
00936 *  MOVE FIRST-PKG-CODE                                     *      ELUMEMBR
00937 *                                                          *      ELUMEMBR
00938 ************************************************************      ELUMEMBR
00939  MOVE-FIRST-PKG-CODE.                                             ELUMEMBR
00940      MOVE 'Y' TO WS-MEM-PKG-FND-SW.                               ELUMEMBR
00941      ADD +1 TO  WS-MBR-ENTRY-CNT.                                 ELUMEMBR
00942 *    SET 4805-MEM-HIST-IDX TO 1.                                  ELUMEMBR
00943      MOVE 0 TO WS-MBR-SCT-NBR-1 (WS-MBR-ENTRY-CNT).               ELUMEMBR
00944      MOVE 470-SECTION-NUMBER TO WS-MBR-SCT-NBR-4                  ELUMEMBR
00945                     (WS-MBR-ENTRY-CNT).                           ELUMEMBR
00946      MOVE 4805-PACKAGE-CODE (4805-MEM-HIST-IDX)                   ELUMEMBR
00947              TO WS-MBR-PKG-CODE (WS-MBR-ENTRY-CNT).               ELUMEMBR
00948       MOVE WS-485-JUL-EFF TO WS-MBR-EFF-DATE-CEN                  ELUMEMBR
00949               (WS-MBR-ENTRY-CNT ).                                ELUMEMBR
00950       IF WS-485-JUL-END = +0                                      ELUMEMBR
00951            MOVE +9999365 TO WS-485-JUL-END                        ELUMEMBR
00952       END-IF.                                                     ELUMEMBR
00953       MOVE WS-485-JUL-END TO WS-MBR-CAN-DATE-CEN                  ELUMEMBR
00954            (WS-MBR-ENTRY-CNT ).                                   ELUMEMBR
00955 *    MOVE 4805-PACKAGE-CODE (4805-MEM-HIST-IDX)  TO               ELUMEMBR
00956 *            WS-MBR-PKG-CODE (WS-MBR-ENTRY-CNT).                  ELUMEMBR
00957                                                                   ELUMEMBR
00958 ************************************************************      ELUMEMBR
00959 *                                                          *      ELUMEMBR
00960 *  SEARCH FOR MULTP PKG DATES                              *      ELUMEMBR
00961 *                                                          *      ELUMEMBR
00962 ************************************************************      ELUMEMBR
00963  SEARCH-FOR-MULT-PKG-DATES.                                       ELUMEMBR
00964      PERFORM 4805-CREATE-CENTURY.                                 ELUMEMBR
00965      IF  (SSB-SRV-FROM-DT-CEN   NOT <                             ELUMEMBR
00966             WS-485-JUL-EFF)                                       ELUMEMBR
00967         AND   (SSB-SRV-FROM-DT-CEN  NOT >                         ELUMEMBR
00968            WS-485-JUL-END)                                        ELUMEMBR
00969            OR (4805-MEM-END-DATE-YMD (4805-MEM-HIST-IDX) = 0)     ELUMEMBR
00970         MOVE 'Y' TO WS-MEM-PKG-FND-SW                             ELUMEMBR
00971         ADD +1 TO WS-MBR-ENTRY-CNT                                ELUMEMBR
00972         MOVE ZEROES TO WS-MBR-SCT-NBR-1 (WS-MBR-ENTRY-CNT)        ELUMEMBR
00973         MOVE 470-SECTION-NUMBER TO WS-MBR-SCT-NBR-4               ELUMEMBR
00974                                    (WS-MBR-ENTRY-CNT)             ELUMEMBR
00975         MOVE 4805-PACKAGE-CODE (4805-MEM-HIST-IDX)                ELUMEMBR
00976                 TO WS-MBR-PKG-CODE (WS-MBR-ENTRY-CNT)             ELUMEMBR
00977           MOVE WS-485-JUL-EFF TO WS-MBR-EFF-DATE-CEN              ELUMEMBR
00978                   (WS-MBR-ENTRY-CNT )                             ELUMEMBR
00979           MOVE WS-485-JUL-END TO WS-MBR-CAN-DATE-CEN              ELUMEMBR
00980                (WS-MBR-ENTRY-CNT )                                ELUMEMBR
00981      END-IF.                                                      ELUMEMBR
00982                                                                   ELUMEMBR
00983                                                                   ELUMEMBR
00984 ************************************************************      ELUMEMBR
00985 *                                                          *      ELUMEMBR
00986 *  HANDLE PACKAGE CODE FOUND/NOT FOUND                     *      ELUMEMBR
00987 *                                                          *      ELUMEMBR
00988 ************************************************************      ELUMEMBR
00989  HANDLE-PKG-CODE-FIND.                                            ELUMEMBR
00990       MOVE ZEROES TO WS-MBR-SCT-NBR-1 (WS-MBR-ENTRY-CNT).         ELUMEMBR
00991       MOVE 470-SECTION-NUMBER TO WS-MBR-SCT-NBR-4 (WS-MBR-IDX).   ELUMEMBR
00992       IF WS-MEM-PKG-FND                                           ELUMEMBR
00993          MOVE 4805-PACKAGE-CODE (4805-MEM-HIST-IDX)               ELUMEMBR
00994                TO WS-MBR-PKG-CODE (WS-MBR-ENTRY-CNT)              ELUMEMBR
00995          MOVE WS-485-JUL-EFF TO WS-MBR-EFF-DATE-CEN               ELUMEMBR
00996                  (WS-MBR-ENTRY-CNT )                              ELUMEMBR
00997          MOVE WS-485-JUL-END TO WS-MBR-CAN-DATE-CEN               ELUMEMBR
00998               (WS-MBR-ENTRY-CNT )                                 ELUMEMBR
00999       ELSE                                                        ELUMEMBR
01000          MOVE 000 TO WS-MBR-PKG-CODE (WS-MBR-ENTRY-CNT)           ELUMEMBR
01001          MOVE WS-HOLD-EFF-DT-CEN TO WS-MBR-EFF-DATE-CEN           ELUMEMBR
01002                  (WS-MBR-ENTRY-CNT )                              ELUMEMBR
01003          MOVE WS-HOLD-EFF-DT-CEN TO WS-MBR-CAN-DATE-CEN           ELUMEMBR
01004               (WS-MBR-ENTRY-CNT )                                 ELUMEMBR
01005       END-IF.                                                     ELUMEMBR
01006                                                                   ELUMEMBR
01007 ************************************************************      ELUMEMBR
01008 *                                                          *      ELUMEMBR
01009 *        MOVE EFFECTIVE AND CANCEL DATES TO MSI BLOCK      *      ELUMEMBR
01010 *                                                          *      ELUMEMBR
01011 ************************************************************      ELUMEMBR
01012  MOVE-EFFECTIVE-AND-CANCEL-DATE.                                  ELUMEMBR
01013      IF MSI-GRP-SUB-EFF-DATE-CC = ZERO OR                         ELUMEMBR
01014                 WS-HOLD-EFF-DT-CEN    <                           ELUMEMBR
01015          MSI-GRP-SUB-EFF-DATE-CC                                  ELUMEMBR
01016          PERFORM INSERT-THE-EARLIEST-EFFECTIVEX.                  ELUMEMBR
01017      MOVE WS-HOLD-CAN-DT-CEN                                      ELUMEMBR
01018               TO MSI-GRP-SUB-TERM-DATE-CENTURY.                   ELUMEMBR
01019                                                                   ELUMEMBR
01020                                                                   ELUMEMBR
01021 ************************************************************      ELUMEMBR
01022 *                                                          *      ELUMEMBR
01023 *        INSERT THE EARLIEST EFFECTIVE DATE                *      ELUMEMBR
01024 *                                                          *      ELUMEMBR
01025 ************************************************************      ELUMEMBR
01026  INSERT-THE-EARLIEST-EFFECTIVEX.                                  ELUMEMBR
01027      MOVE WS-HOLD-EFF-DT-CEN    TO MSI-GRP-SUB-EFF-DATE-CC.       ELUMEMBR
01028                                                                   ELUMEMBR
01029                                                                   ELUMEMBR
01030 ************************************************************      ELUMEMBR
01031 *                                                          *      ELUMEMBR
01032 *        RETAIN INFO FROM LAST RECORD                      *      ELUMEMBR
01033 *                                                          *      ELUMEMBR
01034 ************************************************************      ELUMEMBR
01035  RETAIN-INFO-FROM-LAST-RECORD.                                    ELUMEMBR
01036      IF WS-HOLD-CAN-DT-CEN   > WS-LAST-CAN-DATE-CEN               ELUMEMBR
01037         MOVE 0 TO WS-LAST-SCT                                     ELUMEMBR
01038         MOVE 470-SECTION-NUMBER      TO WS-LAST-SCT-4             ELUMEMBR
01039         MOVE WS-HOLD-EFF-DT-CEN      TO WS-LAST-EFF-DATE-CEN      ELUMEMBR
01040         MOVE WS-HOLD-CAN-DT-CEN      TO WS-LAST-CAN-DATE-CEN.     ELUMEMBR
01041                                                                   ELUMEMBR
01042                                                                   ELUMEMBR
01043 ************************************************************      ELUMEMBR
01044 *                                                          *      ELUMEMBR
01045 *        PROCESS ANY CHAINING THAT EXISTS                  *      ELUMEMBR
01046 *                                                          *      ELUMEMBR
01047 ************************************************************      ELUMEMBR
01048  PROCESS-ANY-CHAINING-THAT-EXIS.                                  ELUMEMBR
01049 ******************************************************************ELUMEMBR
01050 * THIS ROUTINE PERFORMS THE NECESSARY PROCESSING TO FIND THE      ELUMEMBR
01051 * \
01052 * MEMBERSHIP RECORD BEING READ. IT WILL PROCEED IN THAT           ELUMEMBR
01053 * DIRECTION UNTIL IT ENCOUNTERS ANY OF THE FOLLOWING SITUATIONS:  ELUMEMBR
01054 *                                                                 ELUMEMBR
01055 *   1.) NO MORE \
01056 *   2.) THE MEMBER CHANGED TO A DIFFERENT GROUP.                  ELUMEMBR
01057 *   3.) THE MEMBER CHANGED TO A DIFFERENT SUBSCRIBER NUMBER.      ELUMEMBR
01058 *   4.) THE EFFECTIVE AND/OR CANCEL DATES DO NOT FALL IN RANGE    ELUMEMBR
01059 *       SPECIFIED.                                                ELUMEMBR
01060 *   5.) THE CHAIN IS LOOPING (MEMBER WENT BACK TO A SECTION AGAIN)ELUMEMBR
01061 *   6.) THE TABLE IN WORKING STORAGE (WS MEMB TABLE) IS FULL.     ELUMEMBR
01062 *                                          REB ==> 05/26/88       ELUMEMBR
01063 ******************************************************************ELUMEMBR
01064      IF (470-PREV-GROUP    NOT = LOW-VALUES AND SPACES AND        ELUMEMBR
01065          ZEROS)                                                   ELUMEMBR
01066                                       AND                         ELUMEMBR
01067                 (470-PREV-SECTION  NOT = LOW-VALUES AND           ELUMEMBR
01068          SPACES)                                                  ELUMEMBR
01069                                       AND                         ELUMEMBR
01070                 (470-PREV-SUBS     NOT = LOW-VALUES AND SPACES    ELUMEMBR
01071          AND ZEROS)                                               ELUMEMBR
01072          PERFORM PROCEED-WITH-BACKWARD-CHAIN-PR.                  ELUMEMBR
01073      IF (HOLD-TRNSFR-GRP-6   NOT = LOW-VALUES AND SPACES AND      ELUMEMBR
01074          ZEROS)                                                   ELUMEMBR
01075                                       AND                         ELUMEMBR
01076                 (HOLD-TRNSFR-SCT-4 NOT = LOW-VALUES AND           ELUMEMBR
01077          SPACES)                                                  ELUMEMBR
01078                                       AND                         ELUMEMBR
01079                 (HOLD-TRNSFR-MBR   NOT = LOW-VALUES AND SPACES    ELUMEMBR
01080          AND ZEROS)                                               ELUMEMBR
01081          PERFORM PROCEED-WITH-FORWARD-CHAIN-PRO.                  ELUMEMBR
01082 *********************************************************         ELUMEMBR
01083 ** THE CODE BELOW IS TO HANDLE THE SITUATION WHERE THE **         ELUMEMBR
01084 ** INQUIRY DATE RANGE IS AFTER ANY MEMBERSHIP RECORDS. **         ELUMEMBR
01085 **                                 REB ==> 05/26/88    **         ELUMEMBR
01086 *********************************************************         ELUMEMBR
01087      IF WS-MBR-ENTRY-CNT       = ZEROS          AND               ELUMEMBR
01088                 WS-LAST-SCT        NOT = SPACES                   ELUMEMBR
01089          PERFORM MOVE-INFO-FROM-LAST-RECORD-INT.                  ELUMEMBR
01090      PERFORM LOAD-MEMBERSHIP-INTERFACE-TABL                       ELUMEMBR
01091          VARYING MSI-IDX FROM 1 BY 1                              ELUMEMBR
01092                  UNTIL   MSI-IDX > 20                             ELUMEMBR
01093                  OR      MSI-IDX > WS-MBR-ENTRY-CNT.              ELUMEMBR
01094 *    IF TEXAS-REGION AND (WS-MEM-PKG-FND-SW = 'Y')                ELUMEMBR
01095 *        COMPUTE MSI-NBR-MBR-SECTNS = MSI-NBR-MBR-SECTNS - 1      ELUMEMBR
01096 *    END-IF.                                                      ELUMEMBR
01097 ************************************************************      ELUMEMBR
01098 *                                                          *      ELUMEMBR
01099 *        PROCEED WITH BACKWARD CHAIN PROCESSING            *      ELUMEMBR
01100 *                                                          *      ELUMEMBR
01101 ************************************************************      ELUMEMBR
01102  PROCEED-WITH-BACKWARD-CHAIN-PR.                                  ELUMEMBR
01103      PERFORM PROCESS-BACKWARD-CHAIN                               ELUMEMBR
01104          UNTIL STOP-BCKWRD-CHAIN                                  ELUMEMBR
01105                  OR    FILE-LOOP                                  ELUMEMBR
01106                  OR    PREV-KEY-CHANGED                           ELUMEMBR
01107                  OR    WS-MBR-ENTRY-CNT > 20.                     ELUMEMBR
01108                                                                   ELUMEMBR
01109                                                                   ELUMEMBR
01110 ************************************************************      ELUMEMBR
01111 *                                                          *      ELUMEMBR
01112 *        PROCEED WITH FORWARD CHAIN PROCESSING             *      ELUMEMBR
01113 *                                                          *      ELUMEMBR
01114 ************************************************************      ELUMEMBR
01115  PROCEED-WITH-FORWARD-CHAIN-PRO.                                  ELUMEMBR
01116      INITIALIZE WS-LOOP-SW.                                       ELUMEMBR
01117      PERFORM PROCESS-FORWARD-CHAIN                                ELUMEMBR
01118          UNTIL STOP-FWD-CHAIN                                     ELUMEMBR
01119                  OR    FILE-LOOP                                  ELUMEMBR
01120                  OR    TRANS-TO-KEY-CHANGES                       ELUMEMBR
01121                  OR    WS-MBR-ENTRY-CNT > 20.                     ELUMEMBR
01122                                                                   ELUMEMBR
01123                                                                   ELUMEMBR
01124 ************************************************************      ELUMEMBR
01125 *                                                          *      ELUMEMBR
01126 *        PROCESS BACKWARD CHAIN                            *      ELUMEMBR
01127 *                                                          *      ELUMEMBR
01128 ************************************************************      ELUMEMBR
01129  PROCESS-BACKWARD-CHAIN.                                          ELUMEMBR
01130 ******************************************************************ELUMEMBR
01131 * THIS ROUTINE PERFORMS THE READ OF THE \
01132 * ON MEMBERSHIP FILE AND WILL VERIFY THAT GROUP/SUBSCIBER NUMBERS ELUMEMBR
01133 * ARE THE SAME AS SELECTED AND THAT THE DATES FALL IN THE RANGE   ELUMEMBR
01134 * GIVEN. IF IT PASSES ALL THESE CRITERIA IT WILL THEN REARRANGE   ELUMEMBR
01135 * THE WS MEMB TABLE AND MOVE THE CURRENT \
01136 * IN THE FIRST OCCURENCE BECAUSE IT IS THE OLDEST AT THAT POINT.  ELUMEMBR
01137 *                                            REB ==> 05/26/88     ELUMEMBR
01138 ******************************************************************ELUMEMBR
01139      IF 470-PREV-GROUP = SSB-GRP-NO                               ELUMEMBR
01140          PERFORM INDICATE-PREVIOUS-GROUP-MATCHE                   ELUMEMBR
01141      ELSE                                                         ELUMEMBR
01142          PERFORM INDICATE-PREVIOUS-GROUP-NUMBER.                  ELUMEMBR
01143                                                                   ELUMEMBR
01144                                                                   ELUMEMBR
01145 ************************************************************      ELUMEMBR
01146 *                                                          *      ELUMEMBR
01147 *        INDICATE PREVIOUS GROUP MATCHES WITH GROUP SELECTE*      ELUMEMBR
01148 *                                                          *      ELUMEMBR
01149 ************************************************************      ELUMEMBR
01150  INDICATE-PREVIOUS-GROUP-MATCHE.                                  ELUMEMBR
01151      IF 470-PREV-SUBS NOT = SSB-SUBSCRIBER-NBR                    ELUMEMBR
01152         SET  PREV-KEY-CHANGED              TO TRUE                ELUMEMBR
01153         MOVE 470-PREV-SUBS                 TO                     ELUMEMBR
01154             MSI-PREV-MEMBR-NBR                                    ELUMEMBR
01155      ELSE                                                         ELUMEMBR
01156          PERFORM CONTINUE-PROCESSING-BACKWARD-C.                  ELUMEMBR
01157                                                                   ELUMEMBR
01158 ************************************************************      ELUMEMBR
01159 *                                                          *      ELUMEMBR
01160 *        INDICATE PREVIOUS GROUP NUMBER DIFFERS FROM GROUP *      ELUMEMBR
01161 *                                                          *      ELUMEMBR
01162 ************************************************************      ELUMEMBR
01163  INDICATE-PREVIOUS-GROUP-NUMBER.                                  ELUMEMBR
01164      SET  PREV-KEY-CHANGED              TO TRUE.                  ELUMEMBR
01165      MOVE 470-PREV-GROUP                TO MSI-PREV-GRP-NBR.      ELUMEMBR
01166      IF 470-PREV-SUBS NOT = SSB-SUBSCRIBER-NBR                    ELUMEMBR
01167         SET  PREV-KEY-CHANGED              TO TRUE                ELUMEMBR
01168         MOVE 470-PREV-SUBS                 TO                     ELUMEMBR
01169             MSI-PREV-MEMBR-NBR.                                   ELUMEMBR
01170                                                                   ELUMEMBR
01171                                                                   ELUMEMBR
01172 ************************************************************      ELUMEMBR
01173 *                                                          *      ELUMEMBR
01174 *        CONTINUE PROCESSING BACKWARD CHAIN                *      ELUMEMBR
01175 *                                                          *      ELUMEMBR
01176 ************************************************************      ELUMEMBR
01177  CONTINUE-PROCESSING-BACKWARD-C.                                  ELUMEMBR
01178      MOVE 470-PREV-GROUP                TO XIOGRP.                ELUMEMBR
01179      MOVE 470-PREV-SECTION              TO XIOSCT.                ELUMEMBR
01180      MOVE 470-PREV-SUBS                 TO XIOSUB.                ELUMEMBR
01181      PERFORM READ-A-NEW-MEMBERSHIP-RECORD.                        ELUMEMBR
01182      IF XIO-NOT-FOUND                                             ELUMEMBR
01183          SET STOP-BCKWRD-CHAIN           TO TRUE                  ELUMEMBR
01184      ELSE                                                         ELUMEMBR
01185          IF (NOT TEXAS-REGION) OR WS-CHANGE-TX-PKG-CODE-GROUP     ELUMEMBR
01186              CONTINUE                                             ELUMEMBR
01187          ELSE                                                     ELUMEMBR
01188             PERFORM READ-TX-SUPPLEMENTAL                          ELUMEMBR
01189          END-IF                                                   ELUMEMBR
01190          PERFORM CHECK-FOR-LOOP-CONDITION.                        ELUMEMBR
01191      IF NOT FILE-LOOP              AND                            ELUMEMBR
01192                 NOT STOP-BCKWRD-CHAIN      AND                    ELUMEMBR
01193                 WS-MBR-ENTRY-CNT < 20                             ELUMEMBR
01194          PERFORM FINISH-BACKWARD-PROCESS.                         ELUMEMBR
01195                                                                   ELUMEMBR
01196                                                                   ELUMEMBR
01197 ************************************************************      ELUMEMBR
01198 *                                                          *      ELUMEMBR
01199 *        CHECK FOR LOOP CONDITION                          *      ELUMEMBR
01200 *                                                          *      ELUMEMBR
01201 ************************************************************      ELUMEMBR
01202  CHECK-FOR-LOOP-CONDITION.                                        ELUMEMBR
01203      PERFORM CHECK-IF-SECTION-NUMBER-IS-UNI                       ELUMEMBR
01204          VARYING WS-SCT-IDX FROM 1 BY 1                           ELUMEMBR
01205                  UNTIL   WS-SCT-IDX > WS-SECTION-ENTRIES          ELUMEMBR
01206                  OR      FILE-LOOP                                ELUMEMBR
01207                  OR      WS-SECTION-ENTRIES > 50.                 ELUMEMBR
01208      IF NOT FILE-LOOP       AND                                   ELUMEMBR
01209                 WS-SECTION-ENTRIES < 50                           ELUMEMBR
01210          ADD +1                   TO WS-SECTION-ENTRIES           ELUMEMBR
01211          SET 4805-MEM-HIST-IDX TO 1                               ELUMEMBR
01212          MOVE 470-SECTION-NUMBER  TO WS-SECTION-NBR               ELUMEMBR
01213              (WS-SECTION-ENTRIES)                                 ELUMEMBR
01214          IF (NOT TEXAS-REGION) OR WS-CHANGE-TX-PKG-CODE-GROUP     ELUMEMBR
01215              MOVE 000 TO WS-PKG-CODE (WS-SECTION-ENTRIES)         ELUMEMBR
01216          ELSE                                                     ELUMEMBR
01217             MOVE 4805-PACKAGE-CODE (4805-MEM-HIST-IDX) TO         ELUMEMBR
01218                  WS-PKG-CODE (WS-SECTION-ENTRIES)                 ELUMEMBR
01219             SET 4805-MEM-HIST-IDX UP BY 1                         ELUMEMBR
01220             IF 4805-MEM-NO-OF-HISTORY > 1                         ELUMEMBR
01221                MOVE 4805-PACKAGE-CODE (4805-MEM-HIST-IDX) TO      ELUMEMBR
01222                      WS-PKG-CODE (WS-SECTION-ENTRIES)             ELUMEMBR
01223                PERFORM VARYING 4805-MEM-HIST-IDX  FROM            ELUMEMBR
01224                   4805-MEM-HIST-IDX BY 1 UNTIL                    ELUMEMBR
01225                      4805-MEM-HIST-IDX > 4805-MEM-NO-OF-HISTORY   ELUMEMBR
01226                      IF 4805-PACKAGE-CODE (4805-MEM-HIST-IDX  - 1)ELUMEMBR
01227                        = 4805-PACKAGE-CODE (4805-MEM-HIST-IDX)    ELUMEMBR
01228                          CONTINUE                                 ELUMEMBR
01229                      ELSE                                         ELUMEMBR
01230                        ADD +1      TO WS-SECTION-ENTRIES          ELUMEMBR
01231                        MOVE 4805-PACKAGE-CODE (4805-MEM-HIST-IDX) ELUMEMBR
01232                          TO WS-PKG-CODE (WS-SECTION-ENTRIES)      ELUMEMBR
01233                        MOVE 470-SECTION-NUMBER  TO WS-SECTION-NBR ELUMEMBR
01234                            (WS-SECTION-ENTRIES)                   ELUMEMBR
01235                   END-IF                                          ELUMEMBR
01236              END-PERFORM                                          ELUMEMBR
01237             END-IF                                                ELUMEMBR
01238          END-IF                                                   ELUMEMBR
01239      END-IF.                                                      ELUMEMBR
01240                                                                   ELUMEMBR
01241                                                                   ELUMEMBR
01242 ************************************************************      ELUMEMBR
01243 *                                                          *      ELUMEMBR
01244 *        CHECK IF SECTION NUMBER IS UNIQUE                 *      ELUMEMBR
01245 *                                                          *      ELUMEMBR
01246 ************************************************************      ELUMEMBR
01247  CHECK-IF-SECTION-NUMBER-IS-UNI.                                  ELUMEMBR
01248      IF 470-SECTION-NUMBER = WS-SECTION-NBR                       ELUMEMBR
01249          (WS-SCT-IDX)                                             ELUMEMBR
01250          SET FILE-LOOP                   TO TRUE.                 ELUMEMBR
01251                                                                   ELUMEMBR
01252 ************************************************************      ELUMEMBR
01253 *                                                          *      ELUMEMBR
01254 *        FINISH BACKWARD PROCESS                           *      ELUMEMBR
01255 *                                                          *      ELUMEMBR
01256 ************************************************************      ELUMEMBR
01257  FINISH-BACKWARD-PROCESS.                                         ELUMEMBR
01258      PERFORM CONVERT-BOTH-EFFECTIVE-AND-CAN.                      ELUMEMBR
01259      IF WS-HOLD-EFF-DT-CEN > SSB-SRV-TO-DT-CEN       OR           ELUMEMBR
01260                 WS-HOLD-CAN-DT-CEN < SSB-SRV-FROM-DT-CEN          ELUMEMBR
01261          OR                                                       ELUMEMBR
01262                 WS-HOLD-EFF-DT-CEN   = ZEROS                      ELUMEMBR
01263          OR                                                       ELUMEMBR
01264                 WS-HOLD-CAN-DT-CEN   = ZEROS                      ELUMEMBR
01265          PERFORM MOVE-EFFECTIVE-AND-CANCEL-DATE                   ELUMEMBR
01266      ELSE                                                         ELUMEMBR
01267          PERFORM REORGANIZE-WS-MEMB-TABLE.                        ELUMEMBR
01268      IF (470-PREV-GROUP        =  LOW-VALUES OR SPACES OR         ELUMEMBR
01269          ZEROS)                                                   ELUMEMBR
01270                                        OR                         ELUMEMBR
01271                 (470-PREV-SECTION      =  LOW-VALUES OR           ELUMEMBR
01272          SPACES)                                                  ELUMEMBR
01273                                        OR                         ELUMEMBR
01274                 (470-PREV-SUBS         =  LOW-VALUES OR SPACES    ELUMEMBR
01275          OR ZEROS)                                                ELUMEMBR
01276              SET STOP-BCKWRD-CHAIN           TO TRUE.             ELUMEMBR
01277                                                                   ELUMEMBR
01278                                                                   ELUMEMBR
01279 ************************************************************      ELUMEMBR
01280 *                                                          *      ELUMEMBR
01281 *        REORGANIZE WS MEMB TABLE                          *      ELUMEMBR
01282 *                                                          *      ELUMEMBR
01283 ************************************************************      ELUMEMBR
01284  REORGANIZE-WS-MEMB-TABLE.                                        ELUMEMBR
01285 ******************************************************************ELUMEMBR
01286 * THIS CODE WILL BUMP ALL OCCURENCES PRESENTLY IN WS MEMB TABLE UPELUMEMBR
01287 * ONE SO WE CAN INSERT THE CURRENT RECORD IN THE FIRST SLOT.      ELUMEMBR
01288 *                                              REB ==> 05/26/88   ELUMEMBR
01289 ******************************************************************ELUMEMBR
01290      MOVE WS-MBR-ENTRY-CNT          TO SUBX.                      ELUMEMBR
01291      COMPUTE SUBY = SUBX + 1.                                     ELUMEMBR
01292      PERFORM REARRANGE-WS-MEMB-TABLE-IN-COR                       ELUMEMBR
01293          VARYING SUBX FROM SUBX BY -1                             ELUMEMBR
01294                  UNTIL   SUBX = ZERO                              ELUMEMBR
01295                  OR      SUBY > 20                                ELUMEMBR
01296                  OR      SUBY = 1.                                ELUMEMBR
01297 ******************************************************************ELUMEMBR
01298 ** CODE BELOW WILL INSERT THE MOST CURRENT \
01299 ** IN THE FIRST OCCURENCE OF WS MEMB TABLE.    REB ==> 05/26/88   ELUMEMBR
01300 ******************************************************************ELUMEMBR
01301      MOVE ZEROES TO WS-MBR-SCT-NBR-1 (1).                         ELUMEMBR
01302      MOVE 470-SECTION-NUMBER         TO WS-MBR-SCT-NBR-4          ELUMEMBR
01303          (1).                                                     ELUMEMBR
01304      MOVE WS-HOLD-EFF-DT-CEN     TO WS-MBR-EFF-DATE-CEN (1).      ELUMEMBR
01305      MOVE WS-HOLD-CAN-DT-CEN     TO WS-MBR-CAN-DATE-CEN (1).      ELUMEMBR
01306      ADD  +1                     TO WS-MBR-ENTRY-CNT.             ELUMEMBR
01307                                                                   ELUMEMBR
01308                                                                   ELUMEMBR
01309 ************************************************************      ELUMEMBR
01310 *                                                          *      ELUMEMBR
01311 *        REARRANGE WS MEMB TABLE IN CORRECT SEQUENCE       *      ELUMEMBR
01312 *                                                          *      ELUMEMBR
01313 ************************************************************      ELUMEMBR
01314  REARRANGE-WS-MEMB-TABLE-IN-COR.                                  ELUMEMBR
01315      MOVE WS-MBR-SCT-NBR  (SUBX)     TO WS-MBR-SCT-NBR  (SUBY).   ELUMEMBR
01316      MOVE WS-MBR-EFF-DATE-CEN (SUBX)                              ELUMEMBR
01317                      TO WS-MBR-EFF-DATE-CEN (SUBY).               ELUMEMBR
01318      MOVE WS-MBR-CAN-DATE-CEN (SUBX)                              ELUMEMBR
01319                 TO WS-MBR-CAN-DATE-CEN (SUBY).                    ELUMEMBR
01320      SUBTRACT  1  FROM SUBY.                                      ELUMEMBR
01321                                                                   ELUMEMBR
01322                                                                   ELUMEMBR
01323 ************************************************************      ELUMEMBR
01324 *                                                          *      ELUMEMBR
01325 *        READ A NEW MEMBERSHIP RECORD                      *      ELUMEMBR
01326 *                                                          *      ELUMEMBR
01327 ************************************************************      ELUMEMBR
01328  READ-A-NEW-MEMBERSHIP-RECORD.                                    ELUMEMBR
01329 ******************************************************************ELUMEMBR
01330 * THIS ROUTINE PERFORMS THE A READ ON THE MEMBERSHIP FILE BY THE  ELUMEMBR
01331 * KEY SPECIFIED. THIS IS USED BY BOTH BACKWARD AND FORWARD CHAIN  ELUMEMBR
01332 * PROCESSING. (FOR \
01333 * THE REASON XIO-SMF-READ IS SET IS TO ALLOW A DIRECT READ WITH   ELUMEMBR
01334 * THE KEY GIVEN PRIOR TO THIS ROUTINE!       REB ==> 05/26/88     ELUMEMBR
01335 ******************************************************************ELUMEMBR
01336      SET   XIO-SMF-READ      TO TRUE.                             ELUMEMBR
01337      PERFORM SETUP-IO-PARMS-AND-LINK-TO-MEM.                      ELUMEMBR
01338      IF XIO-NO-TRLR-MORE-SECT                                     ELUMEMBR
01339          PERFORM SIGNAL-IO-ERROR                                  ELUMEMBR
01340      ELSE                                                         ELUMEMBR
01341          SET ADDRESS OF MEMBER-HEADER TO XIO-PTR (1).             ELUMEMBR
01342                                                                   ELUMEMBR
01343                                                                   ELUMEMBR
01344 ************************************************************      ELUMEMBR
01345 *                                                          *      ELUMEMBR
01346 *        PROCESS FORWARD CHAIN                             *      ELUMEMBR
01347 *                                                          *      ELUMEMBR
01348 ************************************************************      ELUMEMBR
01349  PROCESS-FORWARD-CHAIN.                                           ELUMEMBR
01350 ******************************************************************ELUMEMBR
01351 * THIS ROUTINE PERFORMS THE READ OF THE \
01352 * ON MEMBERSHIP FILE AND WILL VERIFY THAT GROUP/SUBSCIBER NUMBERS ELUMEMBR
01353 * ARE THE SAME AS SELECTED AND THAT THE DATES FALL IN THE RANGE   ELUMEMBR
01354 * GIVEN. IF IT PASSES ALL THESE CRITERIA IT WILL THEN INSERT THE  ELUMEMBR
01355 * CURRENT \
01356 * WS MEMB TABLE.                           REB ==> 05/26/88       ELUMEMBR
01357 *                                                                 ELUMEMBR
01358 ******************************************************************ELUMEMBR
01359      IF HOLD-TRNSFR-GRP-6 = SSB-GRP-NO                            ELUMEMBR
01360          PERFORM INDICATE-TRANSFER-TO-GRP-NBR-M                   ELUMEMBR
01361      ELSE                                                         ELUMEMBR
01362          PERFORM INDICATE-TRANSFER-TO-GROUP-NUM.                  ELUMEMBR
01363                                                                   ELUMEMBR
01364                                                                   ELUMEMBR
01365 ************************************************************      ELUMEMBR
01366 *                                                          *      ELUMEMBR
01367 *        INDICATE TRANSFER TO GRP NBR MATCHES THE SELECTED *      ELUMEMBR
01368 *                                                          *      ELUMEMBR
01369 ************************************************************      ELUMEMBR
01370  INDICATE-TRANSFER-TO-GRP-NBR-M.                                  ELUMEMBR
01371      IF HOLD-TRNSFR-MBR NOT = SSB-SUBSCRIBER-NBR                  ELUMEMBR
01372          PERFORM SIGNAL-THAT-SUBSCRIBER-NUMBERX                   ELUMEMBR
01373      ELSE                                                         ELUMEMBR
01374          PERFORM CONTINUE-PROCESSING-FORWARD-CH.                  ELUMEMBR
01375                                                                   ELUMEMBR
01376                                                                   ELUMEMBR
01377 ************************************************************      ELUMEMBR
01378 *                                                          *      ELUMEMBR
01379 *        INDICATE TRANSFER TO GROUP NUMBER CHANGED AND SAVE*      ELUMEMBR
01380 *                                                          *      ELUMEMBR
01381 ************************************************************      ELUMEMBR
01382  INDICATE-TRANSFER-TO-GROUP-NUM.                                  ELUMEMBR
01383      SET TRANS-TO-KEY-CHANGES    TO TRUE.                         ELUMEMBR
01384      MOVE HOLD-TRNSFR-GRP-6        TO MSI-NEXT-GRP-NBR.           ELUMEMBR
01385      IF HOLD-TRNSFR-MBR NOT = SSB-SUBSCRIBER-NBR                  ELUMEMBR
01386          PERFORM SIGNAL-THAT-SUBSCRIBER-NUMBERX.                  ELUMEMBR
01387                                                                   ELUMEMBR
01388                                                                   ELUMEMBR
01389 ************************************************************      ELUMEMBR
01390 *                                                          *      ELUMEMBR
01391 *        SIGNAL THAT SUBSCRIBER NUMBER CHANGED             *      ELUMEMBR
01392 *                                                          *      ELUMEMBR
01393 ************************************************************      ELUMEMBR
01394  SIGNAL-THAT-SUBSCRIBER-NUMBERX.                                  ELUMEMBR
01395      SET TRANS-TO-KEY-CHANGES    TO TRUE.                         ELUMEMBR
01396      MOVE HOLD-TRNSFR-MBR        TO MSI-NEXT-MEMBR-NBR.           ELUMEMBR
01397                                                                   ELUMEMBR
01398                                                                   ELUMEMBR
01399 ************************************************************      ELUMEMBR
01400 *                                                          *      ELUMEMBR
01401 *        CONTINUE PROCESSING FORWARD CHAIN                 *      ELUMEMBR
01402 *                                                          *      ELUMEMBR
01403 ************************************************************      ELUMEMBR
01404  CONTINUE-PROCESSING-FORWARD-CH.                                  ELUMEMBR
01405      MOVE HOLD-TRNSFR-GRP-6      TO XIOGRP.                       ELUMEMBR
01406      MOVE HOLD-TRNSFR-SCT-4      TO XIOSCT.                       ELUMEMBR
01407      MOVE HOLD-TRNSFR-MBR        TO XIOSUB.                       ELUMEMBR
01408      PERFORM READ-A-NEW-MEMBERSHIP-RECORD.                        ELUMEMBR
01409      IF XIO-NOT-FOUND                                             ELUMEMBR
01410          SET STOP-FWD-CHAIN TO TRUE                               ELUMEMBR
01411      ELSE                                                         ELUMEMBR
01412          PERFORM CHECK-FOR-LOOP-CONDITION.                        ELUMEMBR
01413      IF NOT FILE-LOOP             AND                             ELUMEMBR
01414                 NOT STOP-FWD-CHAIN        AND                     ELUMEMBR
01415                 WS-MBR-ENTRY-CNT < 20                             ELUMEMBR
01416          PERFORM FINISH-FORWARD-PROCESS.                          ELUMEMBR
01417                                                                   ELUMEMBR
01418                                                                   ELUMEMBR
01419 ************************************************************      ELUMEMBR
01420 *                                                          *      ELUMEMBR
01421 *        FINISH FORWARD PROCESS                            *      ELUMEMBR
01422 *                                                          *      ELUMEMBR
01423 ************************************************************      ELUMEMBR
01424  FINISH-FORWARD-PROCESS.                                          ELUMEMBR
01425      MOVE 470-TRANSFER-TO-NO     TO                               ELUMEMBR
01426          WS-HOLD-TRANSFER-KEY.                                    ELUMEMBR
01427      PERFORM CONVERT-BOTH-EFFECTIVE-AND-CAN.                      ELUMEMBR
01428      PERFORM CHECK-THAT-MEMBERSHIP-RECORD-F.                      ELUMEMBR
01429      IF (470-TRNSFR-GROUP   = LOW-VALUES OR SPACES OR             ELUMEMBR
01430          ZEROS)                                                   ELUMEMBR
01431                                       OR                          ELUMEMBR
01432                   (470-TRNSFR-SECTION = LOW-VALUES OR             ELUMEMBR
01433          SPACES)                                                  ELUMEMBR
01434                                       OR                          ELUMEMBR
01435                   (470-TRNSFR-SUBS    = LOW-VALUES OR SPACES OR   ELUMEMBR
01436          ZEROS)                                                   ELUMEMBR
01437          SET STOP-FWD-CHAIN TO TRUE.                              ELUMEMBR
01438                                                                   ELUMEMBR
01439                                                                   ELUMEMBR
01440 ************************************************************      ELUMEMBR
01441 *                                                          *      ELUMEMBR
01442 *        MOVE INFO FROM LAST RECORD INTO MEMBERSHIP INTERFA*      ELUMEMBR
01443 *                                                          *      ELUMEMBR
01444 ************************************************************      ELUMEMBR
01445  MOVE-INFO-FROM-LAST-RECORD-INT.                                  ELUMEMBR
01446 *    MOVE WS-LAST-SCT          TO MSI-MBR-SECTN (1).              ELUMEMBR
01447      MOVE WS-LAST-SCT          TO MSI-MEMBER-SECTION (1).         ELUMEMBR
01448      IF (NOT TEXAS-REGION)  OR WS-CHANGE-TX-PKG-CODE-GROUP        ELUMEMBR
01449           MOVE 000 TO SSB-PKG-CODE                                ELUMEMBR
01450                       MSI-PKG-CODE (1)                            ELUMEMBR
01451      END-IF.                                                      ELUMEMBR
01452      MOVE WS-LAST-EFF-DATE-CEN                                    ELUMEMBR
01453                   TO MSI-EFF-DATE-CENTURY    (1).                 ELUMEMBR
01454      MOVE WS-LAST-CAN-DATE-CEN                                    ELUMEMBR
01455                       TO MSI-TERMIN-DATE-CC   (1)                 ELUMEMBR
01456                          MSI-GRP-SUB-TERM-DATE-CENTURY.           ELUMEMBR
01457      ADD  +1                   TO MSI-NBR-MBR-SECTNS.             ELUMEMBR
01458                                                                   ELUMEMBR
01459                                                                   ELUMEMBR
01460 ************************************************************      ELUMEMBR
01461 *                                                          *      ELUMEMBR
01462 *        LOAD MEMBERSHIP INTERFACE TABLE                   *      ELUMEMBR
01463 *                                                          *      ELUMEMBR
01464 ************************************************************      ELUMEMBR
01465  LOAD-MEMBERSHIP-INTERFACE-TABL.                                  ELUMEMBR
01466 ******************************************************************ELUMEMBR
01467 * THIS LOGIC MOVES ALL THE ENTRIES IN THE WS MEMB TABLE INTO THE  ELUMEMBR
01468 * MEMBERSHIP INTERFACE BLOCK. IT WILL KEEP TRACK OF ALL ENTRIES   ELUMEMBR
01469 * ENTERED.                                 REB ==> 05/26/88       ELUMEMBR
01470 ******************************************************************ELUMEMBR
01471      MOVE WS-MBR-SCT-NBR    (MSI-IDX) TO MSI-MEMBER-SECTION       ELUMEMBR
01472          (MSI-IDX).                                               ELUMEMBR
01473      MOVE WS-MBR-PKG-CODE (MSI-IDX) TO MSI-PKG-CODE (MSI-IDX).    ELUMEMBR
01474      MOVE WS-MBR-EFF-DATE-CEN (MSI-IDX)                           ELUMEMBR
01475                   TO MSI-EFF-DATE-CENTURY (MSI-IDX).              ELUMEMBR
01476      MOVE WS-MBR-CAN-DATE-CEN (MSI-IDX)                           ELUMEMBR
01477                     TO MSI-TERMIN-DATE-CC (MSI-IDX).              ELUMEMBR
01478      ADD  +1                          TO MSI-NBR-MBR-SECTNS.      ELUMEMBR
01479                                                                   ELUMEMBR
01480                                                                   ELUMEMBR
01481 ************************************************************      ELUMEMBR
01482 *                                                          *      ELUMEMBR
01483 *        CALL CSEXECIO INTERFACE                           *      ELUMEMBR
01484 *                                                          *      ELUMEMBR
01485 ************************************************************      ELUMEMBR
01486  CALL-CSEXECIO-INTERFACE.                                         ELUMEMBR
01487      ADD +1         TO WS-CSEXECIO-LINK-COUNT.                    ELUMEMBR
01488      PERFORM PRESERVE-TWA-AREA.                                   ELUMEMBR
01489      EXEC CICS LINK                                               ELUMEMBR
01490                PROGRAM('CSEXECIO')                                ELUMEMBR
01491                COMMAREA(CSEXECIO-CONTROL)                         ELUMEMBR
01492                END-EXEC.                                          ELUMEMBR
01493      PERFORM RESTORE-TWA-AREA.                                    ELUMEMBR
01494 ****************************************                          ELUMEMBR
01495 * THIS CHECK IS A SAFEQUARD AGAINST A  *                          ELUMEMBR
01496 * POSSIBLE LOOP BY KEEPING TRACK OF    *                          ELUMEMBR
01497 * HOW MANY READS. THE MAXIMUM IS 50    *                          ELUMEMBR
01498 * ACTUAL LINKS.      REB ==> 05/26/88  *                          ELUMEMBR
01499 ****************************************                          ELUMEMBR
01500      IF XIO-INVALID-REQUEST       OR                              ELUMEMBR
01501                 XIO-IO-ERROR              OR                      ELUMEMBR
01502                 XIO-NO-TRLR-NO-SECT       OR                      ELUMEMBR
01503                 MAXIMUM-READ-LIMIT                                ELUMEMBR
01504          PERFORM SIGNAL-IO-ERROR.                                 ELUMEMBR
01505                                                                   ELUMEMBR
01506 *============================================================     ELUMEMBR
01507 *                                                                 ELUMEMBR
01508 *    READ TEXAS SUPPLEMENTAL FILE                                 ELUMEMBR
01509 *                                                                 ELUMEMBR
01510 *============================================================     ELUMEMBR
01511  READ-TX-SUPPLEMENTAL.                                            ELUMEMBR
01512       IF TEXAS-REGION                                             ELUMEMBR
01513          MOVE SSB-GRP-NO TO PSKP-GROUP                            ELUMEMBR
01514          IF NOT WS-CHANGE-TX-PKG-CODE-GROUP                       ELUMEMBR
01515              PERFORM  FIND-MEMBER-PKG-CODE                        ELUMEMBR
01516 * *                 VARYING WS-PKG-SUB1 FROM 1 BY 1               ELUMEMBR
01517 * *             UNTIL WS-PKG-SUB1 > MSI-NBR-MBR-SECTNS            ELUMEMBR
01518            END-IF                                                 ELUMEMBR
01519       END-IF.                                                     ELUMEMBR
01520                                                                   ELUMEMBR
01521 *============================================================     ELUMEMBR
01522                                                                   ELUMEMBR
01523  TEXAS-REGION-CHECK.                                              ELUMEMBR
01524                                                                   ELUMEMBR
01525        IF TEXAS-REGION                                            ELUMEMBR
01526           MOVE SSB-GRP-NO TO PSKP-GROUP                           ELUMEMBR
01527           IF WS-CHANGE-TX-PKG-CODE-GROUP                          ELUMEMBR
01528             MOVE 000 TO SSB-PKG-CODE                              ELUMEMBR
01529           END-IF                                                  ELUMEMBR
01530        END-IF.                                                    ELUMEMBR
01531                                                                   ELUMEMBR
01532 *============================================================     ELUMEMBR
01533 *PKG-CODE-GROUP.                                                  ELUMEMBR
01534 *    MOVE MSI-MBR-SECTN (WS-PKG-SUB1) TO WS-CHECK-SECT            ELUMEMBR
01535 *                                    WS-HOLD-SECT-NO-4.           ELUMEMBR
01536 *    IF WS-SECT-LAST-BYTE = '0'                                   ELUMEMBR
01537 *       PERFORM FIND-MEMBER-PKG-CODE.                             ELUMEMBR
01538 *       PERFORM PKG-LOOKUP                                        ELUMEMBR
01539 *       MOVE WS-HOLD-SECT-NO TO                                   ELUMEMBR
01540 *                     MSI-MEMBER-SECTION (WS-PKG-SUB1)            ELUMEMBR
01541 *    END-IF.                                                      ELUMEMBR
01542                                                                   ELUMEMBR
01543 *============================================================     ELUMEMBR
01544  FIND-MEMBER-PKG-CODE.                                            ELUMEMBR
01545                                                                   ELUMEMBR
01546      INITIALIZE WS-TXSUP-KEY.                                     ELUMEMBR
01547      INITIALIZE 4805-SMF-SUPPLE-RECORD-KEY.                       ELUMEMBR
01548      MOVE SSB-GRP-NO         TO WS-TXSUP-GROUP.                   ELUMEMBR
01549      MOVE WS-HOLD-SECT-NO-4  TO WS-TXSUP-SECT.                    ELUMEMBR
01550      MOVE SSB-SUBSCRIBER-NBR TO WS-TXSUP-SUB-NUMBER.              ELUMEMBR
01551      MOVE  0   TO WS-TXSUP-MEM-NUMBER.                            ELUMEMBR
01552      MOVE '10' TO  WS-TXSUP-REC-TYPE.                             ELUMEMBR
01553                                                                   ELUMEMBR
01554      PERFORM TEXAS-SUPPL-START-BROWSE.                            ELUMEMBR
01555      PERFORM TEXAS-SUPPL-READ-NEXT                                ELUMEMBR
01556 *         UNTIL                                                   ELUMEMBR
01557 *       4805-RECORD-TYPE = '10' OR WS-TXSUP-EOF-SW = 1.           ELUMEMBR
01558 *    MOVE 'N' TO WS-MEM-PKG-FND-SW.                               ELUMEMBR
01559 *    IF WS-TXSUP-EOF-SW = 0                                       ELUMEMBR
01560 *       PERFORM SEARCH-FOR-PKG-DATE                               ELUMEMBR
01561 *    END-IF.                                                      ELUMEMBR
01562      IF TXSUP-BROWSE-SW-ON                                        ELUMEMBR
01563         PERFORM TEXAS-SUPPL-END-BROWSE.                           ELUMEMBR
01564                                                                   ELUMEMBR
01565 *============================================================     ELUMEMBR
01566  SEARCH-FOR-PKG-DATE.                                             ELUMEMBR
01567                                                                   ELUMEMBR
01568      PERFORM 4805-CREATE-CENTURY.                                 ELUMEMBR
01569      PERFORM VARYING 4805-MEM-HIST-IDX FROM 1 BY 1                ELUMEMBR
01570        UNTIL WS-MEM-PKG-FND-SW = 'Y' OR                           ELUMEMBR
01571             4805-MEM-HIST-IDX > 4805-MEM-NO-OF-HISTORY            ELUMEMBR
01572         IF  (SSB-SRV-FROM-DT-CEN   NOT <                          ELUMEMBR
01573                WS-485-JUL-EFF)                                    ELUMEMBR
01574            AND   (SSB-SRV-TO-DT-CEN    NOT >                      ELUMEMBR
01575               WS-485-JUL-END)                                     ELUMEMBR
01576               OR (4805-MEM-END-DATE-YMD (4805-MEM-HIST-IDX) = 0)  ELUMEMBR
01577            MOVE 'Y' TO WS-MEM-PKG-FND-SW                          ELUMEMBR
01578         END-IF                                                    ELUMEMBR
01579      END-PERFORM.                                                 ELUMEMBR
01580                                                                   ELUMEMBR
01581                                                                   ELUMEMBR
01582 *============================================================     ELUMEMBR
01583  4805-CREATE-CENTURY.                                             ELUMEMBR
01584      MOVE ZEROES TO WS-485-HOLD-CC-YY.                            ELUMEMBR
01585      SET WS-485-CONVERT TO TRUE.                                  ELUMEMBR
01586      MOVE 4805-MEM-EFF-YR (4805-MEM-HIST-IDX) TO                  ELUMEMBR
01587           WS-485-HOLD-YY.                                         ELUMEMBR
01588      IF  4805-MEM-EFF-YR (4805-MEM-HIST-IDX) > 70                 ELUMEMBR
01589           MOVE 19 TO WS-485-HOLD-CC                               ELUMEMBR
01590      ELSE                                                         ELUMEMBR
01591           MOVE 20 TO WS-485-HOLD-CC                               ELUMEMBR
01592      END-IF.                                                      ELUMEMBR
01593      MOVE 4805-MEM-EFF-MO (4805-MEM-HIST-IDX) TO                  ELUMEMBR
01594           WS-485-HOLD-MO                                          ELUMEMBR
01595      MOVE 4805-MEM-EFF-DA (4805-MEM-HIST-IDX) TO                  ELUMEMBR
01596           WS-485-HOLD-DA                                          ELUMEMBR
01597      PERFORM LINK-MLDATES.                                        ELUMEMBR
01598      MOVE MLDATE-JUL2 TO WS-485-JUL-EFF.                          ELUMEMBR
01599                                                                   ELUMEMBR
01600      MOVE ZEROES TO WS-485-HOLD-CC-YY.                            ELUMEMBR
01601      IF 4805-MEM-END-DATE-YMD (4805-MEM-HIST-IDX) = 0             ELUMEMBR
01602           MOVE 99991231 TO WS-485-HOLD-CC-YY                      ELUMEMBR
01603      ELSE                                                         ELUMEMBR
01604         MOVE 4805-MEM-END-YR (4805-MEM-HIST-IDX) TO               ELUMEMBR
01605              WS-485-HOLD-YY                                       ELUMEMBR
01606         IF  4805-MEM-END-YR (4805-MEM-HIST-IDX) > 70              ELUMEMBR
01607              MOVE 19 TO WS-485-HOLD-CC                            ELUMEMBR
01608         ELSE                                                      ELUMEMBR
01609              MOVE 20 TO WS-485-HOLD-CC                            ELUMEMBR
01610         END-IF                                                    ELUMEMBR
01611         MOVE 4805-MEM-END-MO (4805-MEM-HIST-IDX) TO               ELUMEMBR
01612              WS-485-HOLD-MO                                       ELUMEMBR
01613         MOVE 4805-MEM-END-DA (4805-MEM-HIST-IDX) TO               ELUMEMBR
01614              WS-485-HOLD-DA                                       ELUMEMBR
01615      END-IF.                                                      ELUMEMBR
01616 *    INITIALIZE WS-485-CONVERT-SW.                                ELUMEMBR
01617      PERFORM LINK-MLDATES.                                        ELUMEMBR
01618      MOVE MLDATE-JUL2 TO WS-485-JUL-END.                          ELUMEMBR
01619      INITIALIZE WS-485-CONVERT-SW.                                ELUMEMBR
01620 *============================================================     ELUMEMBR
01621 *PKG-LOOKUP.                                                      ELUMEMBR
01622                                                                   ELUMEMBR
01623 *    MOVE 'N' TO WS-PKG-DONE-SW.                                  ELUMEMBR
01624 *    PERFORM VARYING GCPCT-IDX FROM 1 BY 1                        ELUMEMBR
01625 *      UNTIL GCPCT-IDX > GCPCT-TABLE-COUNT                        ELUMEMBR
01626 *         OR WS-PKG-DONE-SW = 'Y'                                 ELUMEMBR
01627 *      IF GCPCT-PKG-CODE (GCPCT-IDX) = WS-HOLD-PKG-CODE           ELUMEMBR
01628 *         MOVE GCPCT-SECT-END (GCPCT-IDX) TO WS-SECT-LAST-BYTE    ELUMEMBR
01629 *         MOVE WS-CHECK-SECT TO WS-HOLD-SECT-NO-4                 ELUMEMBR
01630 *         MOVE 'Y' TO WS-PKG-DONE-SW                              ELUMEMBR
01631 *      END-IF                                                     ELUMEMBR
01632 *    END-PERFORM.                                                 ELUMEMBR
01633                                                                   ELUMEMBR
01634                                                                   ELUMEMBR
01635 *===========================================================      ELUMEMBR
01636  TEXAS-SUPPL-START-BROWSE.                                        ELUMEMBR
01637                                                                   ELUMEMBR
01638      IF TXSUP-BROWSE-SW-ON                                        ELUMEMBR
01639         PERFORM TEXAS-SUPPL-END-BROWSE.                           ELUMEMBR
01640                                                                   ELUMEMBR
01641      EXEC CICS STARTBR GTEQ                                       ELUMEMBR
01642                DATASET (WS-TXSUP-DDNAME)                          ELUMEMBR
01643                RIDFLD  (WS-TXSUP-KEY)                             ELUMEMBR
01644                RESP    (WS-TXSUP-RESP1)                           ELUMEMBR
01645                END-EXEC.                                          ELUMEMBR
01646                                                                   ELUMEMBR
01647      IF WS-TXSUP-RESP1 = DFHRESP(NOTFND)                          ELUMEMBR
01648         MOVE  1  TO WS-TXSUP-EOF-SW                               ELUMEMBR
01649         MOVE  0  TO WS-TXSUP-BROWSE-SW                            ELUMEMBR
01650      ELSE                                                         ELUMEMBR
01651         PERFORM CHECK-CICS-RESP                                   ELUMEMBR
01652         MOVE  1  TO WS-TXSUP-BROWSE-SW.                           ELUMEMBR
01653                                                                   ELUMEMBR
01654 *================================================================ ELUMEMBR
01655  TEXAS-SUPPL-END-BROWSE.                                          ELUMEMBR
01656                                                                   ELUMEMBR
01657      IF TXSUP-BROWSE-SW-ON                                        ELUMEMBR
01658         EXEC CICS ENDBR                                           ELUMEMBR
01659                   DATASET (WS-TXSUP-DDNAME)                       ELUMEMBR
01660                   RESP    (WS-TXSUP-RESP1)                        ELUMEMBR
01661              END-EXEC                                             ELUMEMBR
01662         MOVE  0  TO WS-TXSUP-BROWSE-SW                            ELUMEMBR
01663      END-IF.                                                      ELUMEMBR
01664 *                                                                 ELUMEMBR
01665                                                                   ELUMEMBR
01666 *================================================================ ELUMEMBR
01667  TEXAS-SUPPL-READ-NEXT.                                           ELUMEMBR
01668      MOVE 24 TO 4805-MEM-NO-OF-HISTORY.                           ELUMEMBR
01669      EXEC CICS READNEXT                                           ELUMEMBR
01670                DATASET (WS-TXSUP-DDNAME)                          ELUMEMBR
01671                INTO    (4805-SMF-SUPPLEMENTAL-RECORD)             ELUMEMBR
01672                RIDFLD  (WS-TXSUP-KEY)                             ELUMEMBR
01673                RESP    (WS-TXSUP-RESP1)                           ELUMEMBR
01674                END-EXEC.                                          ELUMEMBR
01675                                                                   ELUMEMBR
01676      IF WS-TXSUP-RESP1 = DFHRESP(ENDFILE)                         ELUMEMBR
01677         MOVE  1  TO WS-TXSUP-EOF-SW                               ELUMEMBR
01678      ELSE                                                         ELUMEMBR
01679         PERFORM CHECK-CICS-RESP.                                  ELUMEMBR
01680                                                                   ELUMEMBR
01681      IF 4805-GROUP-NUMBER > SSB-GRP-NO              OR            ELUMEMBR
01682         4805-SECTION-NUMBER > WS-HOLD-SECT-NO-4     OR            ELUMEMBR
01683         4805-SUBSCRIBER-NUMBER > SSB-SUBSCRIBER-NBR OR            ELUMEMBR
01684         4805-MEMBER-NUMBER NOT = 0                                ELUMEMBR
01685            MOVE  1  TO WS-TXSUP-EOF-SW.                           ELUMEMBR
01686                                                                   ELUMEMBR
01687      IF WS-TXSUP-EOF-SW = 0 AND 4805-MEMBER-NUMBER = 0            ELUMEMBR
01688 *       MOVE 'N' TO WS-MEM-PKG-FND-SW                             ELUMEMBR
01689         PERFORM SEARCH-FOR-PKG-DATE                               ELUMEMBR
01690 *       IF  WS-MEM-PKG-FND-SW = 'Y'                               ELUMEMBR
01691            PERFORM TEXAS-PKG-CODE-CHECK                           ELUMEMBR
01692      END-IF.                                                      ELUMEMBR
01693 *===========================================================      ELUMEMBR
01694 *PKG-DATE-CONVERSION.                                             ELUMEMBR
01695 *    MOVE SSB-SRV-FROM-DT TO WS-DATE-JUL.                         ELUMEMBR
01696 **   MOVE MSI-EFF-DT (WS-PKG-SUB1)  TO WS-DATE-JUL.               ELUMEMBR
01697 *    PERFORM CONVERT-TO-YMD-2.                                    ELUMEMBR
01698 *    MOVE HGADATE-DATE2             TO WS-SSB-SRV-FROM-DATE-YMD.  ELUMEMBR
01699 *    MOVE SSB-SRV-TO-DATE TO WS-DATE-JUL.                         ELUMEMBR
01700 *    MOVE MSI-TERM-DT (WS-PKG-SUB1) TO WS-DATE-JUL.               ELUMEMBR
01701 *    PERFORM CONVERT-TO-YMD-2.                                    ELUMEMBR
01702 *    MOVE HGADATE-DATE2             TO WS-SSB-SRV-TO-DATE-YMD.    ELUMEMBR
01703                                                                   ELUMEMBR
01704                                                                   ELUMEMBR
01705 *===========================================================      ELUMEMBR
01706  CONVERT-TO-YMD-2.                                                ELUMEMBR
01707                                                                   ELUMEMBR
01708      MOVE 'CNV'              TO HGADATE-FUNC.                     ELUMEMBR
01709      MOVE 'J'                TO HGADATE-FORM1.                    ELUMEMBR
01710      MOVE 'Y'                TO HGADATE-FORM2.                    ELUMEMBR
01711      IF WS-DATE-JUL = 99365                                       ELUMEMBR
01712         MOVE ZEROES             TO HGADATE-JULIAN1                ELUMEMBR
01713      ELSE                                                         ELUMEMBR
01714          MOVE WS-DATE-JUL        TO HGADATE-JULIAN1               ELUMEMBR
01715      END-IF.                                                      ELUMEMBR
01716      PERFORM CALL-HGADATES-INTERFACE.                             ELUMEMBR
01717      IF HGADATE-RETURN > ZEROES                                   ELUMEMBR
01718          MOVE ZEROS    TO HGADATE-DATE2.                          ELUMEMBR
01719                                                                   ELUMEMBR
01720 ***************************************************************** ELUMEMBR
01721  CHECK-CICS-RESP.                                                 ELUMEMBR
01722 *                                                                 ELUMEMBR
01723      EVALUATE WS-TXSUP-RESP1                                      ELUMEMBR
01724 *00                                                               ELUMEMBR
01725         WHEN DFHRESP(NORMAL)                                      ELUMEMBR
01726            CONTINUE                                               ELUMEMBR
01727 *03                                                               ELUMEMBR
01728         WHEN DFHRESP(NOTFND)                                      ELUMEMBR
01729            CONTINUE                                               ELUMEMBR
01730 *36                                                               ELUMEMBR
01731         WHEN DFHRESP(MAPFAIL)                                     ELUMEMBR
01732            CONTINUE                                               ELUMEMBR
01733                                                                   ELUMEMBR
01734         WHEN DFHRESP(ENDFILE)                                     ELUMEMBR
01735            CONTINUE                                               ELUMEMBR
01736         WHEN OTHER                                                ELUMEMBR
01737            PERFORM SIGNAL-IO-ERROR                                ELUMEMBR
01738      END-EVALUATE.                                                ELUMEMBR
01739                                                                   ELUMEMBR
01740 *************************************************************     ELUMEMBR
01741 ** THIS WILL ESTABLISH A POINTER SO THAT THERE WILL BE     **     ELUMEMBR
01742 ** INFO IN TWA WHEN NEEDED. WHEN INTERFACING WITH CSEXECIO **     ELUMEMBR
01743 ** IT TRASHES THE TWA AREA FOR ITS OWN USES. WE ARE DOING  **     ELUMEMBR
01744 ** THIS RESTORE SO IF ELUABEND IS INVOKED IT HAS AN AREA   **     ELUMEMBR
01745 ** TO WORK WITH.                     REB ==> 06/01/88      **     ELUMEMBR
01746 *************************************************************     ELUMEMBR
01747  PRESERVE-TWA-AREA.                                               ELUMEMBR
01748      SET WS-TWA-PTR  TO TWA-ELSCOMM-PTR.                          ELUMEMBR
01749      SET TWA-ELSCOMM-PTR  TO WS-TWA-HOLD-PTR.                     ELUMEMBR
01750  RESTORE-TWA-AREA.                                                ELUMEMBR
01751      SET WS-TWA-HOLD-PTR  TO TWA-ELSCOMM-PTR.                     ELUMEMBR
01752      SET TWA-ELSCOMM-PTR  TO WS-TWA-PTR.                          ELUMEMBR
01753                                                                   ELUMEMBR
01754                                                                   ELUMEMBR
