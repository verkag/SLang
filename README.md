# SLang

Suspended.

The idea is to create the frontend of the compiler in OCaml and backend in C++.

OCaml is the functional programming language. Like in every other functional programming language it is very convinient to work with tree representaion of data. 

LLVM allows you to easily create the backend of the compiler. 

All you need is to convert OCaml intermediate representation trees to C++ data using protobuf.

