*CICS 53 BATCNTL(OPERV.CICS.BATCNTL) APPLID(A46XAS1P)                   00010000
A46XAS1P,CEMT S FI(GCDATES) CL                                         I00020001
WAIT=00000025                                                           00021001
A46XAS1P,CEMT S FI(GCDATES) DIS                                        A00030001
A46XAS1P,CEMT S FI(GCDATES) DS(HCV.TX.EGENERIC.READONLY.DATE)          A00040001
A46XAS1P,CEMT S FI(GCDATES) CL NOU NOA NOD ENA                         A00050000
*                                                                       00061001
A46XAS1P,CEMT S FI(GCCONTR) CL                                         I00070001
WAIT=00000025                                                           00071001
A46XAS1P,CEMT S FI(GCCONTR) DIS                                        A00080001
A46XAS1P,CEMT S FI(GCCONTR) DS(HCV.TX.EGENERIC.READONLY.CONTRACT)      A00090001
A46XAS1P,CEMT S FI(GCCONTR) CL NOU NOA NOD ENA                         A00100000
*                                                                       00110001
A46XAS1P,CEMT S FI(GCGRPSPC) CL                                        I00120001
WAIT=00000025                                                           00121001
A46XAS1P,CEMT S FI(GCGRPSPC) DIS                                       A00130001
A46XAS1P,CEMT S FI(GCGRPSPC) DS(HCV.TX.EGENERIC.READONLY.GROUPSPC)     A00140001
A46XAS1P,CEMT S FI(GCGRPSPC) CL NOU NOA NOD ENA                        A00150000
*                                                                       00160001
A46XAS1P,CEMT S FI(GCSYSTBL) CL                                        I00170001
WAIT=00000025                                                           00171001
A46XAS1P,CEMT S FI(GCSYSTBL) DIS                                       A00180001
A46XAS1P,CEMT S FI(GCSYSTBL) DS(HCV.TX.GENERIC.READONLY.GCSYSTBL)      A00190001
A46XAS1P,CEMT S FI(GCSYSTBL) CL NOU NOA NOD ENA                        A00200000
