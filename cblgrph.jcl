//CBLGRPH JOB 1
//***************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(CBLGRPH),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(CBLGRPH),DISP=SHR
//***************************************/
// IF RC = 0 THEN
//***************************************/
//RUN       EXEC PGM=CBLGRPH
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//*USAHIST   DD DSN=&SYSUID..VSAM.NCOV19,DISP=SHR
//USAHIST   DD DSN=&SYSUID..NCOV19(COVDAT2),DISP=SHR,
//          DCB=(RECFM=FB,LRECL=130,BLKSIZE=130)
//PRTLINE   DD SYSOUT=*,OUTLIM=1700
//SYSOUT    DD SYSOUT=*,OUTLIM=1700
//CEEDUMP   DD DUMMY
//SYSDUMP   DD DUMMY
//****************************************/
// ELSE
// ENDIF
