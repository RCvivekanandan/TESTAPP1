*********************************************************************** 00001   
* PROGRAM NAME  ===> ELKFLAND                                         * 00002   
* PROGRAM TITLE ===> FUZZY LOGIC - AND                                * 00003   
* AUTHOR        ===> R. LUKETICH                                      * 00004   
* DATE WRITTEN  ===> 27-SEP-1989                                      * 00005   
* FUNCTION      ===> PERFORM A FUZZY LOGIC 'AND' BETWEEN TWO OR MORE  * 00006   
*                    OPERANDS USING FUZZY TRUTH VALUES IN THE RANGE   * 00007   
*                    (-1..+1).                                        * 00008   
*                    A FUZZY LOGIC 'AND' CONSISTS OF THE MINIMUM      * 00009   
*                    VALUE OF THE OPERANDS INVOLVED.  ANY NUMBER OF   * 00010   
*                    OPERANDS MAY BE AND 'ED IN ANY ONE OPERATION,    * 00011   
*                    WITH THE FUZZY 'AND' OF THE VALUES RETURNED IN   * 00012   
*                    THE RESULT FIELD.  NOTE THAT IF NO OPERANDS ARE  * 00013   
*                    SUBMITTED, THE RESULT IS SET TO ZERO             * 00014   
*                    (INDETERMINATE).  IF ONLY ONE OPERAND IS         * 00015   
*                    SUBMITTED, THE VALUE OF THAT OPERAND IS RETURNED * 00016   
*                    AS THE RESULT, I.E., THE RESULT IS THE SAME AS A * 00017   
*                    SIMPLE MOVE OR ASSIGNMENT OF THE OPERAND TO THE  * 00018   
*                    RESULT.                                          * 00019   
*                                                                     * 00020   
* -------------------------- REGISTER USAGE ------------------------- * 00021   
* 00 -                               08 -                             * 00022   
* 01 - PARAMETER LIST BASE ADDRESS   09 -                             * 00023   
* 02 - RESULT ADDRESS                10 -                             * 00024   
* 03 - PARAMETER ADDRESS             11 -                             * 00025   
* 04 -                               12 - POINTER INTO PARAMETER LIST * 00026   
* 05 -                               13 - SAVE AREA ADDRESS           * 00027   
* 06 -                               14 - RETURN ADDRESS              * 00028   
* 07 -                               15 - ENTRY POINT ADDRESS         * 00029   
*                                                                     * 00030   
* F0 - CURRENT RESULT                F4 -                             * 00031   
* F2 -                               F8 -                             * 00032   
*                                                                     * 00033   
* ---------------------------- PARAMTERS ---------------------------- * 00034   
*                                                                     * 00035   
* THIS PROGRAM ASSUMES THAT REGISTER (1) POINTS TO A PARAMETER LIST   * 00036   
* THAT CONTAINS THE FOLLOWING:                                        * 00037   
*                                                                     * 00038   
*              A(RESULT)                                              * 00039   
*              A(OPERAND-1)                                           * 00040   
*              A(OPERAND-2)                                           * 00041   
*                   .                                                 * 00042   
*                   .                                                 * 00043   
*              A(OPERAND-N) (OR'D WITH X'80000000')                   * 00044   
*                                                                     * 00045   
* THE NUMBER OF OPERANDS IS (THEORETICALLY) LIMITED ONLY BY THE       * 00046   
* AMOUNT OF STORAGE AVAILABLE.  THE LAST ENTRY IN THE PARAMETER LIST  * 00047   
* IS IDENTIFIED BY THE FACT THAT THE HIGH ORDER BIT OF THE ADDRESS IS * 00048   
* SET TO '1'.  IBM COBOL LINKAGE CONVENTIONS SET THIS BIT CORRECTLY.  * 00049   
*                                                                     * 00050   
* ----------------------- MAINTENANCE HISTORY ----------------------- * 00051   
*                                                                     * 00052   
*    DATE     LVL  BY                    DESCRIPTION                  * 00053   
* ----------- ---- --- ---------------------------------------------- * 00054   
* 27-SEP-1989 0001 RJL ORIGINAL PROGRAM                               * 00055   
*                                                                     * 00056   
* 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX                    * 00057   
*                             ASM RECOMPILES                          * 00058   
*                                                                     * 00059   
* 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *      00060   
*                                                                *      00061   
* 02.01 23-JAN-2004 AKK RECOMPILE AFTER COMPILER CHANGES         *      00062   
*                                                                *      00063   
* *AKK 12/06/05 REGEN FOR TEST                                        * 00064   
*********************************************************************** 00065   
*                                                                       00066   
PARMLIST EQU   1                                                        00067   
RESULT   EQU   2                                                        00068   
NEXTPARM EQU   3                                                        00069   
PARMPTR  EQU   12                                                       00070   
RETADD   EQU   14                                                       00071   
BASEREG  EQU   15                                                       00072   
CURRESLT EQU   0                                                        00073   
ELKFLAND CSECT                                                          00074   
         USING *,BASEREG                                                00075   
         SAVE  (14,12)                 SAVE CALLER'S REGISTERS          00076   
         LE    CURRESLT,=E'0E+00'      SET DEFAULT RESULT VALUE         00077   
         L     RESULT,0(PARMLIST)      GET RESULT ADDRESS               00078   
         LR    PARMPTR,PARMLIST        SET POINTER INTO PARAMETER LIST  00079   
         TM    0(PARMPTR),X'80'        CHECK FOR END OF PARAMETER LIST  00080   
         BNZ   STORESLT                STORE RESULT IF DONE             00081   
         LE    CURRESLT,=E'1E+00'      SET INITIAL RESULT TO TRUE       00082   
*                                                                       00083   
NEXTOP   LA    PARMPTR,4(PARMPTR)      POINT TO NEXT PARM LIST ENTRY    00084   
         L     NEXTPARM,0(PARMPTR)     GET ADDRESS OF NEXT LOGIC VALUE  00085   
         CE    CURRESLT,0(NEXTPARM)    COMPARE VALUES                   00086   
         BL    TESTEND                 IF CURR < NEXT, SKIP             00087   
         LE    CURRESLT,0(NEXTPARM)    ELSE REPLACE CURR                00088   
TESTEND  TM    0(PARMPTR),X'80'        CHECK FOR END OF LIST            00089   
         BZ    NEXTOP                  LOOP IF NOT END                  00090   
STORESLT STE   CURRESLT,0(RESULT)      ELSE STORE RESULT                00091   
         RETURN (14,12)                RETURN TO CALLER                 00092   
         LTORG                                                          00093   
         END                                                            00094   
