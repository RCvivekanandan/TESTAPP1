00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELP033  
00003  PROGRAM-ID.         ELP033.                                         LV001
00004                                                                   ELP033  
00005  AUTHOR.             EDWARD G LISS.                               ELP033  
00006                                                                   ELP033  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELP033  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELP033  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELP033  
00010                      233 N. MICHIGAN AVE                          ELP033  
00011                      CHICAGO, ILLINOIS 60601                      ELP033  
00012                                                                   ELP033  
00013  DATE-WRITTEN.       11-JUL-1989.                                 ELP033  
00014                                                                   ELP033  
00015  DATE-COMPILED.                                                   ELP033  
00016                                                                   ELP033  
00017  SECURITY.           COPYRIGHT 1989,                              ELP033  
00018                      HEALTH CARE SERVICE CORPORATION              ELP033  
00019      SKIP3                                                        ELP033  
00020 ******************************************************************ELP033  
00021 *                                                                *ELP033  
00022 *                    ELS ABEND PROCESSING                        *ELP033  
00023 *                                                                *ELP033  
00024 *   THIS SUBROUTINE PRINTS THE KTG (GROUP KEY TABLE) FOR ELS     *ELP033  
00025 *   ABEND PROCESSING.  THE ENTIRE KTBG IS PRINTED BY THIS        *ELP033  
00026 *   MODULE.                                                      *ELP033  
00027 *                                                                *ELP033  
00028 ******************************************************************ELP033  
00029 *                                                                *ELP033  
00030 *                      MAINTENANCE HISTORY                       *ELP033  
00031 *                                                                *ELP033  
00032 *  MOD     DATE     BY  DRPT                ACTION               *ELP033  
00033 * ----- ----------- --- ----- ---------------------------------- *ELP033  
00034 * 01.00 11-JUL-1989 EGL       CREATED.                           *ELP033  
00035 *                                                                *ELP033  
00036 ******************************************************************ELP033  
00037  TITLE 'ELS ABEND PROCESSING - INITIALIZE SNAP SHOT FILE'.        ELP033  
00038  ENVIRONMENT DIVISION.                                            ELP033  
00039                                                                   ELP033  
00040  CONFIGURATION SECTION.                                           ELP033  
00041  SOURCE-COMPUTER.    IBM-3090.                                    ELP033  
00042  OBJECT-COMPUTER.    IBM-3090.                                    ELP033  
00043                                                                   ELP033  
00044  INPUT-OUTPUT SECTION.                                            ELP033  
00045  FILE-CONTROL.                                                    ELP033  
00046      SELECT SNAP-SHOT-FILE                                        ELP033  
00047          ASSIGN TO UT-AS-SNAPSHOT                                 ELP033  
00048          ACCESS MODE IS SEQUENTIAL                                ELP033  
00049          ORGANIZATION IS SEQUENTIAL.                              ELP033  
00050                                                                   ELP033  
00051  DATA DIVISION.                                                   ELP033  
00052  FILE SECTION.                                                    ELP033  
00053  FD  SNAP-SHOT-FILE.                                              ELP033  
00054      COPY ELSNAPSC.                                               ELP033  
00055 /                                                                 ELP033  
00056  PROCEDURE DIVISION.                                              ELP033  
00057      OPEN OUTPUT SNAP-SHOT-FILE.                                  ELP033  
00058      INITIALIZE SSR-FIXED-PORTION.                                ELP033  
00059      MOVE 1 TO SSR-SUB-SIZE.                                      ELP033  
00060      WRITE SSR-SNAP-SHOT-RECORD.                                  ELP033  
00061      CLOSE SNAP-SHOT-FILE.                                        ELP033  
00062      MOVE ZERO TO RETURN-CODE.                                    ELP033  
00063      GOBACK.                                                      ELP033  
