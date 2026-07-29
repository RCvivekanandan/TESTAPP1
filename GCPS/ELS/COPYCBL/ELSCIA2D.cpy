******************************************************************      00010000
*                                                                *      00020000
*    COPYBOOK:   ELSCIA2D                                        *      00030000
*    DATE:       17-JUN-1988                                     *      00040000
*    AUTHOR:     EDWARD G. LISS                                  *      00050000
*    FUNCTION:   COMMON INTERFACE CONTROL BLOCK FOR ELS ENGLISH  *      00060000
*                CONTRACT INQUIRY PROGRAMS.                      *      00070000
*                                                                *      00080000
*                DEFINES INTERFACE TO INPUT/OUTPUT AND STORAGE   *      00090000
*                MANAGEMENT SUBROUTINES USED IN ELS.  ALSO       *      00100000
*                CONTAINS A TABLE INDICATING WHETHER A PARTICU-  *      00110000
*                LAR SELECTOR IS A KEY OR A TOPIC SELECTOR.      *      00120000
*                THIS TABLE MUST BE SYNCHRONIZED WITH THE MODULE *      00130000
*                STATUS TABLE IN THE SELECTOR STATUS CONTROL     *      00140000
*                BLOCK (ELSSSCB).                                *      00150000
*                                                                *      00160000
*                THIS IS A TOTAL REWORK OF ELSCIAC TO SUPPORT    *      00170000
*                A MORE FLEXIBLE STORAGE MANAGEMENT.             *      00180000
*                                                                *      00190000
*   UNTIL THE TRANSITION TO THE NEW STORAGE MANAGEMENT IS        *      00190101
*   COMPLETED, IF YOU MAKE ANY CHANGES TO ANY ONE OF THE         *      00190201
*   FOLLOWING MEMBERS, YOU MUST MAKE SURE THAT THE CORRESPONDING *      00190301
*   CHANGE IS MADE IN ALL OTHER MEMBERS IN THIS GROUP:           *      00190401
*                                                                *      00190501
*     ELSCIAC  - COMMON INTERFACE AREA (OLD VERSION)             *      00190601
*     ELSCIA2C - COMMON INTERFACE AREA (NEW VERSION)             *      00190701
*     ELSCIA2D - COMMON INTERFACE AREA (NEW ASSEMBLER VERSION)   *      00190801
*     ELSSMAC  - STORAGE MANAGEMENT CONTROL TABLE (NEW VERSION)  *      00190901
*                                                                *      00191001
******************************************************************      00200000
*                                                                *      00210000
*                      MAINTENANCE HISTORY                       *      00220000
*                                                                *      00230000
*  MOD     DATE     BY  DRPT                ACTION               *      00240000
* ----- ----------- --- ----- ---------------------------------- *      00250000
* 02.00 14-SEP-1988 EGL       CREATED ELSCIA2D                   *      00260000
* 02.01 25-OCT-1993 RJL       ADDED NEW MEMBER TO TSQ STOW LIST  *      00261001
*                                                                *      00270000
******************************************************************      00280000
*                                                                       00290000
CIA      DSECT                                                          00300000
CIADDN   DS    CL8            CIA-DDNAME                                00310000
CIALEN   DS    XL4            CIA-AREA-LEN                              00320000
CIAFUNC  DS    X              CIA-STG-MGT-FCN                           00330000
CIAFUNCS DS    X              CIA-STG-MGT-FNC-SAVE                      00340000
CIALEN16 DS    XL2            CIA-AREA-LEN-16M                          00350000
CIABCODE DS    CL4            CIA-ABCODE                                00360000
CIATSQID DS    CL8            CIA-TSQ-ID                                00370000
CIATSQIT DS    XL2            CIA-TSQ-ITEM-NBR                          00380000
CIATSQLN DS    XL2            CIA-TSQ-TS-LEN                            00390000
CIATSQPT DS    XL4            CIA-TSQ-TS-PTR                            00400000
CIATSQMX DS    XL2            CIA-TSQ-MAX                               00410001
CIATSQTB DS    8CL8           CIA-TSQ-TABLE                             00420001
CIARTNCD DS    XL2            CIA-RETURN-CODE                           00430000
CIAMVO   DS    XL2            CIA-MVO                                   00440000
         DS    25CL1          CIA-SEL-TYP-FLAGS                         00441000
******************************************************************      00442000
******************************************************************      00443000
********* DO NOT USE THIS TABLE -  FOR COMPATABILITY ONLY ********      00444000
******************************************************************      00445000
******************************************************************      00446000
CIASMA   DS    XL2            CIA-NBR-STG-MGT-BLKS                      00447000
         DS    68CL20         CIA-STG-MGT-TBL                           00448000
******************************************************************      00449000
******************************************************************      00450000
********* DO NOT USE THIS TABLE -  FOR COMPATABILITY ONLY ********      00460000
******************************************************************      00470000
******************************************************************      00480000
