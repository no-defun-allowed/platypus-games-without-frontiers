# Platypus Games Without Frontiers

This is my capstone that I did under the supervision of
[Prof. James Harland](http://titan.csit.rmit.edu.au/~e24991/) in Semester 2 of 2023.

This project uses extensions specific to the
[Steel Bank Common Lisp](http://sbcl.org/) implementation, mostly in the
SB-SIMD library for writing single instruction-multiple data code. 

There are two systems written in Common Lisp: most of the code is in
the `platypus-games-without-frontiers` system, and the distributed
computing server is in the `platypus-magic` system.

The file names of chapters in the report (in the `Report/` directory) mostly
coincide with the file names of the file names of relevant code (in the
`Code/` directory). Exceptions to this rule are:

- Distributed-host/: the server for distributed computing.
- OpenCL-interpreter/: the client for distributed computing.
- layout.lisp: a layout engine for rendering Platypus machines.
- representation.lisp: functions for manipulating Platypus machines.
- vector.lisp: a package which provides more concise aliases for
  functions in the SB-SIMD library.
- vm.lisp: an extension to the SBCL backend to provide atomic operations
  on 32-bit data.
