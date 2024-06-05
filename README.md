# HotGlue-lang

A language for simply gluing things together, because duct tape and screws are overrated.
<hr>

## The intention

Often, implementing functionality is much easier in an isolated context, compared to within a large application.
And it does makes sense, since everything has to, not just work, but work *together* in harmony and effieciently.
However this is not fun because it restraints bigger projects. With HotGlue-lang I want to test how much
of the implementation of the interplay can be hidden behind a declarative descprition of the desired behaviour.

## The idea

HotGlue assumes that every functionality you want to include might require some *ingredients* and might produce some
*products*, which themselves could be ingredients for other functions. In that sense it is just a functional model.
These two *ingredients* and *products* represent all of the information about the functionality that is available to HotGlue,
in addition to the source file location, function name and source language.\
For this to work, a really good type system is necessary. Such that it is easy to distribute and select these products.
This type system should also be trivially translatable to C-types.\
\
The declarative elements of HotGlue find use in describing behaviour. For instance the keyword `req` poses a boolean
requirement on any statement and the keyword `trigger` activates an imperative procedure at any time on a boolean state
change to true.\
A HotGlue script contains at least one instance of the imperative keyword `do`, which is similarly important to the
main function in C, but instead of representing the *entry* point. Think of it as the *exit*, because it declares the
last *product* that will be produced by the program, the final state. It is the job of HotGlue to figure out how to get
there, while juggling all the requirements and sideeffects. This is how HotGlue lang can be usefull.\
\
In this basic example, a frame with the attribute Time set to 10 Seconds, which must be produced by the `last` Renderer,
which is the last Renderer object in the execution tree (if a single one exists), is saved to disk. Where `save_image` is
just a wrapper for a C function. `last from all` avoids ambiguities if there are multiple renderers for different objects,
which feed into each other.
```c
do save_image("frame/path.png", last Frame(Time: 10 * Second) from all Renderer)
```
This description of the final state will in a sense *collaps* the description of the behaviour (the HotGlue program)
into an executable thing.

## Building on Windows

Run the `build.bat` script from the Microsoft Visual Studio _"x64 Native Tools Command Prompt"_ or from any command prompt with the vcvars64.bat environment.\
Alternatively, a prebuilt windows executable is already located in the build directory.
