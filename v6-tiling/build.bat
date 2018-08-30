:::::::::::::::::::::::::::
:: COMPILER AND STANDARD ::
:::::::::::::::::::::::::::

@echo off

set CMP=ifort
set STD=/stand:f08

@echo on

:::::::::::
:: FLAGS ::
:::::::::::

@echo off

set CMPFLAGS=/Qopenmp /Qdiag-disable:7025,6477
set OPTFLAGS=/O2 /QxAVX
::set OPTFLAGS=/O2 /QxCORE-AVX2
::set OPTFLAGS=/O2 /QxCORE-AVX512 /Qopt-zmm-usage:high
set REPFLAGS=/debug /Qopt-report=5 /Qopt-report-filter="gsimulation.f90,313-415"

set FLAGS=%STD% %CMPFLAGS% %OPTFLAGS% %REPFLAGS%

@echo on

:::::::::::
:: FILES ::
:::::::::::

@echo off

set SRC=mt19937_64.f90 cputime.f90 types.f90 particle.f90 gsimulation.f90 main.f90
set OBJ=mt19937_64.obj cputime.obj types.obj particle.obj gsimulation.obj main.obj

set EXE=nbody.exe
set ADV=adv-v6-tiling

@echo on

::::::::::::
:: SWITCH ::
::::::::::::

@if %1.==. (
    call :all
) else (
    set ERRORMESSAGE=Rule "%1" not found
    2>nul call :%1
)

@if errorlevel 1 (
    echo %ERRORMESSAGE%
    exit /B 1
)

@exit /B 0

::::::::::::
:: CHECKS ::
::::::::::::

:check-compiler
    @where %CMP% >nul 2>nul
    @if errorlevel 1 (
        set ERRORMESSAGE=%CMP% compiler not found. Please install the compiler or check PATH environment variable if installed.
        exit /B 1
    )

    @exit /B 0

:check-advisor
    @where advixe-cl >nul 2>nul
    @if errorlevel 1 (
        set ERRORMESSAGE=advixe-cl not found. Please install Intel Advisor or check PATH environment variable if installed
        exit /B 1
    )

    @where advixe-gui >nul 2>nul
    @if errorlevel 1 (
        set ERRORMESSAGE=advixe-gui not found. Please install Intel Advisor or check PATH environment variable if installed
        exit /B 1
    )

    @exit /B 0

:check-exec
    @if not exist %EXE% (
        set ERRORMESSAGE=%EXE% not found. Run without arguments to build %EXE%.
        exit /B 1
    )

    @exit /B 0

:check-advixeproj
    @if not exist %ADV% (
        set ERRORMESSAGE=%ADV% folder not found. Run "survey" or "roofline" to collect the data.
        exit /B 1
    )

    @exit /B 0

:::::::::::
:: RULES ::
:::::::::::

:all
    @call :clean
    @call :cpu
    @if errorlevel 1 (
        exit /B 1
    )

    @exit /B 0

:cpu
    @call :check-compiler
    @if errorlevel 1 (
        exit /B 1
    )

    @echo.
    @echo ### Compiling the source files for CPU:
    %CMP% %FLAGS% /c %SRC%

    @echo.
    @echo ### Linking the CPU executable:
    %CMP% %FLAGS% %OBJ% /link /out:%EXE%

    @exit /B 0

:run
    @call :check-exec
    @if errorlevel 1 (
        exit /B 1
    )

    @echo.
    @echo ### Running the CPU executable:
    @%EXE%

    @exit /B 0

:asm
    @call :check-compiler
    @if errorlevel 1 (
        exit /B 1
    )

    @echo.
    @echo ### Generating assembly code:
    %CMP% %FLAGS% /S gsimulation.f90

    @exit /B 0

:clean
    @echo.
    @echo ### Removing temporary files:
    del %EXE% %OBJ% *.mod *.asm *.optrpt *.pdb *.ilk

    @exit /B 0

:::::::::::::
:: ADVISOR ::
:::::::::::::

:survey
    @call :check-advisor
    @if errorlevel 1 (
        exit /B 1
    )

    @call :check-exec
    @if errorlevel 1 (
        exit /B 1
    )

    advixe-cl -collect survey -project-dir %ADV% -- %EXE%

    @exit /B 0

:roofline
    @call :check-advisor
    @if errorlevel 1 (
        exit /B 1
    )

    @call :check-exec
    @if errorlevel 1 (
        exit /B 1
    )

    advixe-cl -collect survey -project-dir %ADV% -- %EXE% 
    advixe-cl -collect tripcounts -flop -project-dir %ADV% -- %EXE%

    @exit /B 0

:map
    @call :check-advisor
    @if errorlevel 1 (
        exit /B 1
    )

    @call :check-exec
    @if errorlevel 1 (
        exit /B 1
    )

    advixe-cl -collect map -mark-up-list=1 -project-dir %ADV% -- %EXE%

    @exit /B 0

:dependencies
    @call :check-advisor
    @if errorlevel 1 (
        exit /B 1
    )

    @call :check-exec
    @if errorlevel 1 (
        exit /B 1
    )

    advixe-cl -collect dependencies -mark-up-list=1 -project-dir %ADV% -- %EXE% 1000 1

    @exit /B 0

:open-gui
    @call :check-advisor
    @if errorlevel 1 (
        exit /B 1
    )

    @call :check-advixeproj
    @if errorlevel 1 (
        exit /B 1
    )

    start /B advixe-gui %ADV%\%ADV%.advixeproj >nul 2>nul

    @exit /B 0

:clean-results
    if exist %ADV% rd /S /Q %ADV%

    @exit /B 0

:eof
