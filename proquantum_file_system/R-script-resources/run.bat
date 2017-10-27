@echo off

set R_PATH=%1
echo R_PATH:%1

set fileName=%2
echo fileName:%2

echo %*

setlocal EnableDelayedExpansion

rem Number of arguments to skip
set skip=2

for %%a in (%*) do (
  if not !position! lss !skip! (
    set args=!args! %%a
  ) else (
    set /a "position=!position!+1"
  )
)

echo %args%

%R_PATH% %fileName% %args%

endlocal



rem echo all:%*
rem set RESTVAR=
rem shift
rem :loop1
rem if "%1"=="" goto after_loop
rem set RESTVAR=%RESTVAR% %2
rem shift 
rem goto loop1

rem :after_loop
rem echo afterShift:%RESTVAR%

rem %R_PATH% %fileName% %args%

rem "C:\Program Files\R\R-3.4.1\bin\Rscript.exe" C:\Users\ashishr\Desktop\R_WorkSpace\input\deprendo-5pl-aws-driver.r C:\Users\ashishr\Desktop\R_WorkSpace\input\deprendo-5pl-aws.r C:\Users\ashishr\Desktop\R_WorkSpace\input\ct-results.csv C:\Users\ashishr\Desktop\R_WorkSpace\input\plate-setup.csv C:\Users\ashishr\Desktop\R_WorkSpace\input\prism-export.R C:\Users\ashishr\Desktop\R_WorkSpace\input\conc-results.tml NA 5 basic 15,70-130 eds NA RG-96 C:\Users\ashishr\Desktop\R_WorkSpace\output

pause
