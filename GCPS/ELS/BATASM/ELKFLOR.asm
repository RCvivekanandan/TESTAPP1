*********************************************************************** 00001   
*                                                                     * 00002   
* PROGRAM NAME  ===> ELKFLOR                                          * 00003   
* PROGRAM TITLE ===> FUZZY LOGIC - OR                                 * 00004   
* AUTHOR        ===> R. LUKETICH                                      * 00005   
* DATE WRITTEN  ===> 27-SEP-1989                                      * 00006   
* FUNCTION      ===> PERFORM A FUZZY LOGIC 'OR' BETWEEN TWO OR MORE   * 00007   
*                    OPERANDS USING FUZZY TRUTH VALUES IN THE RANGE   * 00008   
*                    (-1..+1).                                        * 00009   
*                                                                     * 00010   
*                    A FUZZY LOGIC 'OR' CONSISTS OF THE MAXIMUM       * 00011   
*                    VALUE OF THE OPERANDS INVOLVED.  ANY NUMBER OF   * 00012   
*                    OPERANDS MAY BE OR'ED IN ANY ONE OPERATION,      * 00013   
*                    WITH THE FUZZY 'OR' OF THE VALUES RETURNED IN    * 00014   
*                    THE RESULT FIELD.  NOTE THAT IF NO OPERANDS ARE  * 00015   
*                    SUBMITTED, THE RESULT IS SET TO ZERO             * 00016   
*                    (INDETERMINATE).  IF ONLY ONE OPERAND IS         * 00017   
*                    SUBMITTED, THE VALUE OF THAT OPERAND IS RETURNED * 00018   
*                    AS THE RESULT, I.E., THE RESULT IS THE SAME AS A * 00019   
*                    SIMPLE MOVE OR ASSIGNMENT OF THE OPERAND TO THE  * 00020   
*                    RESULT.                                          * 00021   
*                    RESULT.                                          * 00022   
* AKK 12/06/05 REGEN FOR TEST                                         * 00023   
* -------------------------- REGISTER USAGE ------------------------- * 00024   
* 00 -                               08 -                             * 00025   
* 01 - PARAMETER LIST BASE ADDRESS   09 -                             * 00026   
* 02 - RESULT ADDRESS                10 -                             * 00027   
* 03 - PARAMETER ADDRESS             11 -                             * 00028   
* 04 -                               12 - POINTER INTO PARAMETER LIST * 00029   
* 05 -                               13 - SAVE AREA ADDRESS           * 00030   
* 06 -                               14 - RETURN ADDRESS              * 00031   
* 07 -                               15 - ENTRY POINT ADDRESS         * 00032   
*                                                                     * 00033   
* F0 - CURRENT RESULT                F4 -                             * 00034   
* F2 -                               F8 -                             * 00035   
*                                                                     * 00036   
*                                                                     * 00037   
* ---------------------------- PARAMTERS ---------------------------- * 00038   
*                                                                     * 00039   
* THIS PROGRAM ASSUMES THAT REGISTER (1) POINTS TO A PARAMETER LIST   * 00040   
* THAT CONTAINS THE FOLLOWING:                                        * 00041   
*                                                                     * 00042   
*              A(RESULT)                                              * 00043   
*              A(OPERAND-1)                                           * 00044   
*              A(OPERAND-2)                                           * 00045   
*                   .                                                 * 00046   
*                   .                                                 * 00047   
*              A(OPERAND-N) (OR'D WITH X'80000000')                   * 00048   
*                                                                     * 00049   
* THE NUMBER OF OPERANDS IS (THEORETICALLY) LIMITED ONLY BY THE       * 00050   
* AMOUNT OF STORAGE AVAILABLE.  THE LAST ENTRY IN THE PARAMETER LIST  * 00051   
* IS IDENTIFIED BY THE FACT THAT THE HIGH ORDER BIT OF THE ADDRESS IS * 00052   
* SET TO '1'.  IBM COBOL LINKAGE CONVENTIONS SET THIS BIT CORRECTLY.  * 00053   
*                                                                     * 00054   
* ----------------------- MAINTENANCE HISTORY ----------------------- * 00055   
*                                                                     * 00056   
*    DATE     LVL  BY                    DESCRIPTION                  * 00057   
* ----------- ---- --- ---------------------------------------------- * 00058   
* 29-SEP-1989 0001 RJL ORIGINAL PROGRAM                               * 00059   
*                                                                     * 00060   
* 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX                    * 00061   
*                             ASM RECOMPILES                          * 00062   
*                                                                     * 00063   
* 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *      00064   
*                                                                *      00065   
* 02.01 23-JAN-2004 AKK       HAD TO REGEN AS JOHN L. FIXED    *        00066   
*                             CICSCB3 COMPILER FIX             *        00067   
*                                                                     * 00068   
*********************************************************************** 00069   
*                                                                       00070   
PARMLIST EQU   1                                                        00071   
RESULT   EQU   2                                                        00072   
NEXTPARM EQU   3                                                        00073   
PARMPTR  EQU   12                                                       00074   
RETADD   EQU   14                                                       00075   
BASEREG  EQU   15                                                       00076   
CURRESLT EQU   0                                                        00077   
ELKFLOR  CSECT                                                          00078   
         USING *,BASEREG                                                00079   
         SAVE  (14,12)                 SAVE CALLER'S REGISTERS          00080   
         LE    CURRESLT,=E'0E+00'      SET DEFAULT RESULT VALUE         00081   
         L     RESULT,0(PARMLIST)      GET RESULT ADDRESS               00082   
         LR    PARMPTR,PARMLIST        SET POINTER INTO PARAMETER LIST  00083   
         TM    0(PARMPTR),X'80'        CHECK FOR END OF PARAMETER LIST  00084   
         BNZ   STORESLT                STORE RESULT IF DONE             00085   
         LE    CURRESLT,=E'-1E+00'      SET INITIAL RESULT TO FALSE     00086   
*                                                                       00087   
NEXTOP   LA    PARMPTR,4(PARMPTR)      POINT TO NEXT PARM LIST ENTRY    00088   
         L     NEXTPARM,0(PARMPTR)     GET ADDRESS OF NEXT LOGIC VALUE  00089   
         CE    CURRESLT,0(NEXTPARM)    COMPARE VALUES                   00090   
         BH    TESTEND                 IF CURR > NEXT, SKIP             00091   
         LE    CURRESLT,0(NEXTPARM)    ELSE REPLACE CURR                00092   
TESTEND  TM    0(PARMPTR),X'80'        CHECK FOR END OF LIST            00093   
         BZ    NEXTOP                  LOOP IF NOT END                  00094   
STORESLT STE   CURRESLT,0(RESULT)      ELSE STORE RESULT                00095   
         RETURN (14,12)                RETURN TO CALLER                 00096   
         LTORG                                                          00097   
         END                                                            00098   
