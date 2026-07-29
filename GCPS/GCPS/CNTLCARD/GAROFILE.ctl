*CICS 03 BATCNTL(OPERV.CICS.BATCNTL) APPLID(A46FILEP)                   00010002
*                                                                       00020002
A46FILEP,CEMT S FI(GCBENPUP) CL DIS                                    I00030004
WAIT=00000025                                                           00040002
A46FILEP,CEMT S FI(GCBENPUP) DS(HCGENV.EGENERIC.READONLY.BENPROV)      A00060002
A46FILEP,CEMT S FI(GCBENPUP) CL NOU NOA NOD ENA                        A00070002
*                                                                       00080002
A46FILEP,CEMT S FI(GCPRVTAB) CL DIS                                    I00090004
WAIT=00000025                                                           00100002
A46FILEP,CEMT S FI(GCPRVTAB) DS(HCV.GENERIC.GCPRVTB2)                  A00120002
A46FILEP,CEMT S FI(GCPRVTAB) CL NOU NOA NOD ENA                        A00130002
*                                                                       00140002
A46FILEP,CEMT S FI(GCSLOT) CL DIS                                      I00150004
*                                                                       00180002
A46FILEP,CEMT S FI(GCTABLUP) CL DIS                                    I00190004
WAIT=00000025                                                           00200002
A46FILEP,CEMT S FI(GCTABLUP) DS(HCGENV.EGENERIC.READONLY.TABULAR)      A00220002
A46FILEP,CEMT S FI(GCTABLUP) CL NOU NOA NOD ENA                        A00230002
