*CICS 53 BATCNTL(OPERV.CICS.BATCNTL) APPLID(A46XAS1P)                   00010006
A46XAS1P,CEMT S FI(GCDATES) CL                                         I00020008
WAIT=00000025                                                           00021008
A46XAS1P,CEMT S FI(GCDATES) DIS                                        A00030008
A46XAS1P,CEMT S FI(GCDATES) DS(HCV.TX.EGENERIC.DATE)                   A00040008
A46XAS1P,CEMT S FI(GCDATES) CL NOU NOA NOD ENA                         A00050006
*                                                                       00061008
A46XAS1P,CEMT S FI(GCCONTR) CL                                         I00070008
WAIT=00000025                                                           00071008
A46XAS1P,CEMT S FI(GCCONTR) DIS                                        A00080008
A46XAS1P,CEMT S FI(GCCONTR) DS(HCV.TX.EGENERIC.CONTRACT)               A00090008
A46XAS1P,CEMT S FI(GCCONTR) CL NOU NOA NOD ENA                         A00100006
*                                                                       00110008
A46XAS1P,CEMT S FI(GCGRPSPC) CL                                        I00120008
WAIT=00000025                                                           00121008
A46XAS1P,CEMT S FI(GCGRPSPC) DIS                                       A00130008
A46XAS1P,CEMT S FI(GCGRPSPC) DS(HCV.TX.EGENERIC.GROUPSPC)              A00140008
A46XAS1P,CEMT S FI(GCGRPSPC) CL NOU NOA NOD ENA                        A00150006
*                                                                       00160008
A46XAS1P,CEMT S FI(GCSYSTBL) CL                                        I00170008
WAIT=00000025                                                           00171008
A46XAS1P,CEMT S FI(GCSYSTBL) DIS                                       A00180008
A46XAS1P,CEMT S FI(GCSYSTBL) DS(HCV.TX.GENERIC.GCSYSTBL)               A00190008
A46XAS1P,CEMT S FI(GCSYSTBL) CL NOU NOA NOD ENA                        A00200006
