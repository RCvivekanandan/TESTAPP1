*CICS 10 BATCNTL(OPERV.CICS.BATCNTL.FILE3) APPLID(A46MAS1P)             00010001
*                                                                       00020002
A46MAS1P,CEMT S FI(GCDATES) CL DIS                                     I00030004
WAIT=00000025                                                           00040002
A46MAS1P,CEMT S FI(GCDATES) DS(HCV.EGENERIC.READONLY.DATE)             A00060002
A46MAS1P,CEMT S FI(GCDATES) CL NOU NOA NOD ENA                         A00070001
*                                                                       00080002
A46MAS1P,CEMT S FI(GCCONTR) CL DIS                                     I00090004
WAIT=00000025                                                           00100002
A46MAS1P,CEMT S FI(GCCONTR) DS(HCV.EGENERIC.READONLY.CONTRACT)         A00120002
A46MAS1P,CEMT S FI(GCCONTR) CL NOU NOA NOD ENA                         A00130001
*                                                                       00140002
A46MAS1P,CEMT S FI(GCGRPSPC) CL DIS                                    I00150004
WAIT=00000025                                                           00160002
A46MAS1P,CEMT S FI(GCGRPSPC) DS(HCV.EGENERIC.READONLY.GROUPSPC)        A00180002
A46MAS1P,CEMT S FI(GCGRPSPC) CL NOU NOA NOD ENA                        A00190001
