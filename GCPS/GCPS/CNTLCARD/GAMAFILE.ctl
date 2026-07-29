*CICS 03 BATCNTL(OPERV.CICS.BATCNTL) APPLID(A46FILEP)                   00010002
*                                                                       00020002
A46FILEP,CEMT S FI(GCBENPUP) CL DIS                                    I00030002
WAIT=00000025                                                           00040002
A46FILEP,CEMT S FI(GCBENPUP) CL ADD UPD DEL ENA                        A00050002
*                                                                       00060002
A46FILEP,CEMT S FI(GCPRVTAB) CL DIS                                    I00070002
WAIT=00000025                                                           00080002
A46FILEP,CEMT S FI(GCPRVTAB) CL ADD UPD DEL ENA                        A00090002
*                                                                       00100002
A46FILEP,CEMT S FI(GCSLOT) CL DIS                                      I00110002
WAIT=00000025                                                           00120002
A46FILEP,CEMT S FI(GCSLOT) CL ADD UPD DEL ENA                          A00130002
*                                                                       00140002
A46FILEP,CEMT S FI(GCTABLUP) CL DIS                                    I00150002
WAIT=00000025                                                           00160002
A46FILEP,CEMT S FI(GCTABLUP) CL ADD UPD DEL ENA                        A00170002
