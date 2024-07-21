# HotGlue-lang

A language for gluing things together.
<hr>

## The intention

The project is an experiment for exploring the usefullness of a language, made just for running functions from
other languages. The data types and functions as well as the target result are described in the language, a path
to the target result will then be generated.\

HotGlue targets heterogenous and accelerated computing, which is reflected in the supported languages:
CUDA, OpenCL and C.

## How it works

After a source file is included, it's functions can be called with normal arguments and/or constant arguments.
The constant arguments are incorporated during compilation of the source file.\
The function is expected to mutate it's arguments or return an object.\
Types can be defined for interfacing between the functions.\
Declarative requirements and imperative logic define the programm-flow.\
All objects exist from the beginning to the end of the execution.\
New objects can be added or bigger changes made at runtime in the interactive mode with Hot recompilation.\

## Building on Windows

Run the `build.bat` script from the Microsoft Visual Studio _"x64 Native Tools Command Prompt"_ or from any command prompt with the `vcvars64.bat` environment.\
Alternatively, a prebuilt windows executable is already located in the build directory.
