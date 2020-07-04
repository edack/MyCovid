//COVIDAJ JOB 1
//***************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(COVID19A),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(COVID19A),DISP=SHR
//***************************************/
// IF RC = 0 THEN
//***************************************/
//RUN       EXEC PGM=COVID19A
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//CNTRYREC  DD DSN=&SYSUID..NCOV19(COVDAT1),DISP=SHR,
//          DCB=(RECFM=FB,LRECL=130,BLKSIZE=130)
//PRTLINE   DD SYSOUT=*,OUTLIM=17000
//SYSOUT    DD SYSOUT=*,OUTLIM=17000
//CEEDUMP   DD DUMMY
//SYSDUMP   DD DUMMY
//****************************************/
// ELSE
// ENDIF
