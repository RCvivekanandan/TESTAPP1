         TITLE 'ELAIOFMN - ENGLISH LANGUAGE GENERALIZED FREEMAIN MODULEX00001   
                '                                                       00002   
ELAIOFMN DFHEIENT CODEREG=(3,4,5)                                       00003   
***************************************************************         00004   
*        M O D I F I C A T I O N S   L O G                    *         00005   
***************************************************************         00006   
*   MOD#    DATE     NAME        DESCRIPTION                  *         00007   
*   ------  -------- ----------- -----------------------------*         00008   
*           11/28/84 B WOLOWIEC  INITIAL INSTALLATION         *         00009   
*           02/26/86 D SECOR     CONVERTED FOR ENGLISH LANG.  *         00010   
*                                                             *         00011   
***************************************************************         00012   
***************************************************************         00013   
*                                                             *         00014   
*   PROGRAM:      ELAIOFMN                                    *         00015   
*   AUTHOR:       BOB I/O WOLOWIEC                            *         00016   
*   DATE WRITTEN: 11/15/84                                    *         00017   
*                                                             *         00018   
*   PURPOSE:   PROVIDE I/O SUPPORT FOR THE HCMS-G SYSTEM.     *         00019   
*                                                             *         00020   
*                                                             *         00021   
*   FUNCTIONS: WHEN LINKED TO                                 *         00022   
*              THIS MODULE WILL FREEMAIN THE GETMAIN AREAS    *         00023   
*              OBTAINED THRU ELAIOGMN.                        *         00024   
*08/14/03 NEED NEW LEVEL TO OVERRIDE PROD RC 12-- W/O THIS    *         00025   
*         MOVE TO PROD WON'T BE MADE                          *         00026   
***************************************************************         00027   
         EJECT                                                          00028   
***************************************************************         00029   
*        R E G I S T E R     U S A G E                        *         00030   
***************************************************************         00031   
R0       EQU   0    LINKAGE/WORK                                        00032   
R1       EQU   1    LINKAGE/WORK                                        00033   
R2       EQU   2    WORK                                                00034   
R3       EQU   3    1ST BASE REGISTER                                   00035   
R4       EQU   4    2ND BASE REGISTER                                   00036   
R5       EQU   5    3RD BASE REGISTER                                   00037   
R6       EQU   6    COMMON INTERFACE AREA FROM CALLER                   00038   
R7       EQU   7    COMMON I/O PARM AREA FROM CALLER                    00039   
R8       EQU   8    RECORD ADDRESS POINTER                              00040   
R9       EQU   9    SECOND RECORD ADDR. POINTER FOR ADDS                00041   
R10      EQU   10   WORK                                                00042   
R11      EQU   11   EXEC INTERFACE POINTER                              00043   
R12      EQU   12   BAL REGISTER FOR SUBROUTINE PROCESSES               00044   
R13      EQU   13   DFHEISTG DYNAMIC STORAGE AREA                       00045   
R14      EQU   14   LINKAGE/WORK                                        00046   
R15      EQU   15   LINKAGE/WORK                                        00047   
         EJECT                                                          00048   
         COPY  ELADCIA                                                  00049   
         EJECT                                                          00050   
         COPY  ELADIOPM                                                 00051   
         EJECT                                                          00052   
*                                                                       00053   
*  DSECTS FOR SETTING ADDRESSABILITY FOR INSTRUCTIONS                   00054   
*                                                                       00055   
ELIOREC  DSECT                                                          00056   
ELIOREC2 DSECT                                                          00057   
         EJECT                                                          00058   
DFHEISTG DSECT                                                          00059   
ABCODE   DS    CL4                ABEND CODE                            00060   
GTMAINL  DS    H                                                        00061   
*                                                                       00062   
         EJECT                                                          00063   
*                                                                       00064   
ELAIOFMN CSECT                                                          00065   
         PROGDATE                                                       00066   
         SPACE                                                          00067   
         CLC   EIBCALEN,=H'4'      EXPECTED COMMAREA LENGTH             00068   
         BNE   INVREQ                                                   00069   
         SPACE                                                          00070   
         L     R6,DFHEICAP         GET THE COMMAREA ADDRESS             00071   
         L     R6,0(,R6)           GET THE COMMON INTERFACE AREA        00072   
*    ESTABLISH ADDRESSABILIBY TO THE CIA AREA                           00073   
         USING ELADCIA,R6           PASSED FROM CALLER.                 00074   
         SPACE                                                          00075   
*  ESTABLISH ADDRESSABILITY TO I/O PARM AREA                            00076   
         SPACE                                                          00077   
*  WE MUST HAVE AN I/O PARM AREA.                                       00078   
         L     R7,CIAIOINT                                              00079   
         LTR   R7,R7                                                    00080   
         BZ    INVREQ                                                   00081   
         SPACE                                                          00082   
         USING ELADIOPM,R7                                              00083   
         SPACE 2                                                        00084   
*      FREE THE RECORD AREA FIRST                                       00085   
         SPACE                                                          00086   
*   CHECK THAT THE RECORD IN I/O PARMS HAS A VALUE, IF SO FREE          00087   
         L     R8,ELIORECA                                              00088   
         LTR   R8,R8                                                    00089   
         BZ    FREEIOPM                                                 00090   
         SPACE                                                          00091   
         SPACE 2                                                        00092   
         EXEC  CICS FREEMAIN DATA(0(R8))                                00093   
         SPACE 2                                                        00094   
FREEIOPM EQU   *                                                        00095   
*        FREE THE I/O PARM AREA.                                        00096   
         SPACE                                                          00097   
         EXEC  CICS FREEMAIN DATA(0(R7))                                00098   
         SPACE 2                                                        00099   
*   CLEAR THE I/O PARM ADDRESS IN THE CALLERS CIA.                      00100   
*   WE DON'T WANT IT USED AGAIN SINCE THE STORAGE IS FREED.             00101   
         MVC   CIAIOINT,=4X'00'                                         00102   
         EJECT                                                          00103   
*        ************************                                       00104   
*        *  RETURN TO CALLER    *                                       00105   
*        ************************                                       00106   
RETURN   DS    0H                                                       00107   
         SPACE 2                                                        00108   
         EXEC  CICS RETURN                                              00109   
         EJECT                                                          00110   
**********************************************                          00111   
*       INVALID FUNCTION REQUESTED - ABEND   *                          00112   
**********************************************                          00113   
        SPACE                                                           00114   
INVREQ   EQU   *                                                        00115   
         SPACE                                                          00116   
         MVC   ABCODE,=C'INVR'                                          00117   
         SPACE 2                                                        00118   
         EXEC  CICS ABEND ABCODE(ABCODE)                                00119   
         EJECT                                                          00120   
         LTORG                                                          00121   
         END                                                            00122   
