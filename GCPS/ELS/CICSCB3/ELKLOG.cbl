00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELKLOG  
00003  PROGRAM-ID.         ELKLOG.                                         LV002
00004                                                                   ELKLOG  
00005  AUTHOR.             ANNE KEFFER KING.                            ELKLOG  
00006                                                                   ELKLOG  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELKLOG  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELKLOG  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELKLOG  
00010                      233 N. MICHIGAN AVE                          ELKLOG  
00011                      CHICAGO, ILLINOIS 60601                      ELKLOG  
00012                                                                   ELKLOG  
00013  DATE-WRITTEN.       13-MAR-1990.                                 ELKLOG  
00014                                                                   ELKLOG  
00015  DATE-COMPILED.                                                   ELKLOG  
00016                                                                   ELKLOG  
00017  SECURITY.           COPYRIGHT 1990,                              ELKLOG  
00018                      HEALTH CARE SERVICE CORPORATION              ELKLOG  
00019      SKIP3                                                        ELKLOG  
00020 ******************************************************************ELKLOG  
00021 *                                                                *ELKLOG  
00022 *    PROGRAM:    ELKLOG                                          *ELKLOG  
00023 *    DATE:       13-MAR-1990                                     *ELKLOG  
00024 *    AUTHOR:     ANNE KEFFER KING                                *ELKLOG  
00025 *    FUNCTION:                                                   *ELKLOG  
00026 *      THIS MODULE WILL LOAD DATA INTO THE ELSNAPC RECORD AND    *ELKLOG  
00027 *      WRITE THE RECORD TO THE ELSNAPS FILE.  THIS LOG SUB-      *ELKLOG  
00028 *      ROUTINE IS BEING USED TO CAPTURE THE 'NOT FOUND'          *ELKLOG  
00029 *      CODE VALUES AND DATA ELEMENTS FROM CODE MANUAL, BUT       *ELKLOG  
00030 *      ITS USE MAY BE EXPANDED.                                  *ELKLOG  
00031 *                                                                *ELKLOG  
00032 ******************************************************************ELKLOG  
00033 *                                                                *ELKLOG  
00034 *                      MAINTENANCE HISTORY                       *ELKLOG  
00035 *                                                                *ELKLOG  
00036 *  MOD     DATE     BY  DRPT                ACTION               *ELKLOG  
00037 * ----- ----------- --- ----- ---------------------------------- *ELKLOG  
00038 * 01.00 13-MAR-1990 AKK       CREATED                            *ELKLOG  
00039 * 01.00 12-AUG-2003 AKK       GEN'D TO TEST ORDER OF COMPILE     *ELKLOG  
00040 ******************************************************************ELKLOG  
00041                                                                   ELKLOG  
00042  ENVIRONMENT DIVISION.                                            ELKLOG  
00043                                                                   ELKLOG  
00044  CONFIGURATION SECTION.                                           ELKLOG  
00045  SOURCE-COMPUTER.    IBM-3033.                                    ELKLOG  
00046  OBJECT-COMPUTER.    IBM-3033.                                    ELKLOG  
00047  TITLE 'SNAPSHOT FILE LOG SUBROUTINE  MODULE'.                    ELKLOG  
00048  DATA DIVISION.                                                   ELKLOG  
00049                                                                   ELKLOG  
00050  WORKING-STORAGE SECTION.                                         ELKLOG  
00051 *                                                                 ELKLOG  
00052  01  WS-MISC.                                                     ELKLOG  
00053      05  WS-SNAPSHOT-FILE-ID       PIC X(08)  VALUE 'ELSSNAPS'.   ELKLOG  
00054      05  WS-RBA                    PIC S9(08) COMP.               ELKLOG  
00055      05  WS-MOVE-LEN               PIC S9(08) COMP.               ELKLOG  
00056      05  WS-DUMP-POS               PIC S9(08) COMP.               ELKLOG  
00057      05  WS-PAD-CHAR               PIC X(01) VALUE ' '.           ELKLOG  
00058 *                                                                 ELKLOG  
00059 *                                                                 ELKLOG  
00060  COPY ELSNAPSC.                                                   ELKLOG  
00061 /                                                                 ELKLOG  
00062  LINKAGE SECTION.                                                 ELKLOG  
00063  01  DFHCOMMAREA.                                                 ELKLOG  
00064  COPY ELSCOMMC.                                                   ELKLOG  
00065 /                                                                 ELKLOG  
00066  COPY ELSCIA2C.                                                   ELKLOG  
00067 /                                                                 ELKLOG  
00068 *                                                                 ELKLOG  
00069  COPY ELSELOGC.                                                   ELKLOG  
00070 /                                                                 ELKLOG  
00071  01  LS-RECORD.                                                   ELKLOG  
00072      05  LS-BYTE               OCCURS 4039 TIMES                  ELKLOG  
00073                                PIC X(01).                         ELKLOG  
00074 /                                                                 ELKLOG  
00075 *                                                                 ELKLOG  
00076  PROCEDURE DIVISION USING DFHEIBLK                                ELKLOG  
00077                           DFHCOMMAREA.                            ELKLOG  
00078      PERFORM 0000-INITIALIZATION.                                 ELKLOG  
00079      PERFORM 1000-PROCESS-LOG.                                    ELKLOG  
00080      GOBACK.                                                      ELKLOG  
00081                                                                   ELKLOG  
00082  0000-INITIALIZATION.                                             ELKLOG  
00083      PERFORM 0010-VALIDATE-COMMAREA.                              ELKLOG  
00084      PERFORM 0020-ESTABLISH-ADDRESS-OF-LOG.                       ELKLOG  
00085      INITIALIZE SSR-FIXED-PORTION.                                ELKLOG  
00086      SET SSR-MAX-SUB-SIZE TO TRUE.                                ELKLOG  
00087      SET SSR-AREA-PTR TO NULLS.                                   ELKLOG  
00088                                                                   ELKLOG  
00089  0010-VALIDATE-COMMAREA.                                          ELKLOG  
00090      IF LENGTH OF DFHCOMMAREA NOT = EIBCALEN                      ELKLOG  
00091          PERFORM 9000-SIGNAL-INVALID-COMMAREA.                    ELKLOG  
00092      CALL 'ELUINISM' USING DFHCOMMAREA                            ELKLOG  
00093              ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.            ELKLOG  
00094                                                                   ELKLOG  
00095  0020-ESTABLISH-ADDRESS-OF-LOG.                                   ELKLOG  
00096      SET CIA-ELSELOG-DDN TO TRUE.                                 ELKLOG  
00097      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELKLOG  
00098                            ADDRESS OF LG-LOG-RECORD.              ELKLOG  
00099      IF CIA-RC-OK                                                 ELKLOG  
00100          MOVE CIA-DDNAME TO SSR-DDNAME                            ELKLOG  
00101      ELSE                                                         ELKLOG  
00102         PERFORM 9010-SIGNAL-AREA-MISSING.                         ELKLOG  
00103                                                                   ELKLOG  
00104  1000-PROCESS-LOG.                                                ELKLOG  
00105      PERFORM 1100-LOAD-FIXED-PORTION.                             ELKLOG  
00106      SET SSR-AREA-PTR TO ADDRESS OF LG-LOG-RECORD.                ELKLOG  
00107      PERFORM 1150-LOAD-SNAPSHOT-RECORD.                           ELKLOG  
00108      EXEC CICS WRITE                                              ELKLOG  
00109         DATASET(WS-SNAPSHOT-FILE-ID)                              ELKLOG  
00110         FROM(SSR-SNAP-SHOT-RECORD)                                ELKLOG  
00111         RIDFLD (WS-RBA)                                           ELKLOG  
00112         RBA                                                       ELKLOG  
00113      END-EXEC.                                                    ELKLOG  
00114                                                                   ELKLOG  
00115  1100-LOAD-FIXED-PORTION.                                         ELKLOG  
00116      MOVE LG-RECORD-TYPE TO SSR-RECORD-TYPE.                      ELKLOG  
00117      PERFORM 1110-OBTAIN-SYSTEM-APPPLIC-ID.                       ELKLOG  
00118      MOVE EIBDATE TO SSR-ABEND-DATE.                              ELKLOG  
00119      MOVE EIBTIME TO SSR-ABEND-TIME.                              ELKLOG  
00120      MOVE EIBTRMID TO SSR-TERMINAL-ID.                            ELKLOG  
00121      MOVE LG-LOG-LENGTH TO SSR-AREA-LENGTH.                       ELKLOG  
00122 *                          SSR-SUB-AREA.                          ELKLOG  
00123                                                                   ELKLOG  
00124  1110-OBTAIN-SYSTEM-APPPLIC-ID.                                   ELKLOG  
00125      EXEC CICS ASSIGN                                             ELKLOG  
00126           SYSID(SSR-CICS-SYSTEM-ID)                               ELKLOG  
00127      END-EXEC.                                                    ELKLOG  
00128      EXEC CICS ASSIGN                                             ELKLOG  
00129           APPLID(SSR-CICS-APPL-ID)                                ELKLOG  
00130      END-EXEC.                                                    ELKLOG  
00131                                                                   ELKLOG  
00132  1150-LOAD-SNAPSHOT-RECORD.                                       ELKLOG  
00133       MOVE 1 TO WS-DUMP-POS.                                      ELKLOG  
00134       SET ADDRESS OF LS-RECORD TO SSR-AREA-PTR.                   ELKLOG  
00135       MOVE LG-LOG-LENGTH TO WS-MOVE-LEN.                          ELKLOG  
00136       CALL 'ELUMVCL' USING LS-BYTE(WS-DUMP-POS)                   ELKLOG  
00137                            WS-MOVE-LEN                            ELKLOG  
00138                            SSR-SUB-REC(1)                         ELKLOG  
00139                            WS-MOVE-LEN                            ELKLOG  
00140                            WS-PAD-CHAR.                           ELKLOG  
00141 /                                                                 ELKLOG  
00142  9000-SIGNAL-INVALID-COMMAREA.                                    ELKLOG  
00143      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELKLOG  
00144      EXEC CICS ABEND                                              ELKLOG  
00145                ABCODE(CIA-ABCODE)                                 ELKLOG  
00146                END-EXEC.                                          ELKLOG  
00147                                                                   ELKLOG  
00148  9010-SIGNAL-AREA-MISSING.                                        ELKLOG  
00149      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELKLOG  
00150      EXEC CICS ABEND                                              ELKLOG  
00151                ABCODE(CIA-ABCODE)                                 ELKLOG  
00152                END-EXEC.                                          ELKLOG  
