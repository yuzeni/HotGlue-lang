@echo off
setlocal enabledelayedexpansion

set tracy_profiling=0
set tracy_path=C:\src\Tracy-0.10\tracy-0.10\public

set exe_name=HotGlue.exe
set defines=/D HG_PLATFORM_windows=1 /D HG_DEBUG=1 /D HG_LOG_ERRORS=1 /D HG_LOG_WARNINGS=1 /D HG_LOG_INFO=1 /D HG_ENABLE_LOG_COLORS=1
set src_files=..\src\lexer.cpp ..\src\parser.cpp ..\src\test_parser.cpp ..\src\utils.cpp ..\src\ast.cpp ..\src\semantics.cpp ..\src\tdop_functions.cpp
rem /W4 /Wall /O2
set CFlags=/std:c++20 /EHsc /MD /Zi
set include_paths

IF %tracy_profiling% NEQ 0 (
   set include_paths=%include_paths% /I%tracy_path%
   set src_files=%src_files% %tracy_path%\TracyClient.cpp
   set defines=%defines% /D TRACY_ENABLE /D TRACY_CORSE=1 /D TRACY_NO_EXIT=1
   IF %tracy_profiling% EQU 2 (
      set defines=!defines! /D TRACY_ALL=1
   )
)

mkdir build
pushd build
del %exe_name%
cl %CFlags% %defines% %include_paths% %src_files% /Fe:%exe_name%
.\%exe_name%
popd

endlocal
