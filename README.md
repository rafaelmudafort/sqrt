# SQRT

A basic square root calculation program written in Fortran which provides a basis for evaluating various software infrastructure tools
and ideas.

Methods demonstrated here:

- [Compiler directives with CMake](#compiler-directives-with-cmake)
- [Unit testing with pFUnit](#unit-testing-with-pfunit)
- [Unit testing with Python](#unit-testing-with-python)
- [Debugging](#debugging)
- [Linking Fortran and C](#linking-fortran-and-c)
- [Continuous integration with TravisCI](#continuous-integration-with-travisci)
- [Continuous integration with GitHub Actions](#continuous-integration-with-github-actions)

### Compiler directives with CMake

This can be done simply with something like

In CMake
```CMake
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Dcmake_flag")
```

In Fortran code
```FOrtran
#if (defined(cmake_flag))
  write(*,*) "cmake_flag", cmake_flag
#endif
```

However, it is important to ensure that the Fortran file is preprocessed. Files ending in `F90` are automatically preprocessed, but all
files can be preprocessed with the appropriate compiler flag: `-cpp` for gfortran.
See the [GNU preprocessor options](https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gfortran/Preprocessing-Options.html#Preprocessing-Options) for more info.

### Unit testing with pFUnit

The unit testing framework I chose to evaluate and implement here is [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit)
developed at NASA Goddard. It has turned out to be nice for developing tests within a single framework. Specifically, when doing
development in Fortran it is convenient to stay in Fortran to write the unit tests.

However, I have found it to be limiting in scope and a bit of a heavy lift for the functionality that you get. A better approach
may be to use compile the Fortran project as a shared library and drive the unit tests with a Python framework.

### Unit testing with Python

As mentioned in the section above, pFUnit is a bit of a heavy lift for simple unit testing. So, I wanted to try linking a Fortran
library to Python since I already have the C bindings exposed.

This whole process is pretty straightforward using the `ctypes` library. As long as the Fortran library is compiled correctly
with C bindings, you can simply link and use it in Python like this:

```Python
from ctypes import CDLL
import numpy as np

newtonraphsonlib = CDLL('./build/libnewtonraphson_fortran.dylib')
np.testing.assert_almost_equal(
  newtonraphsonlib.int_2x(4),
  8
)
```

### Debugging

Debugging Fortran on mac can be tricky as `gdb` isn't well supported. Intel provides `gdb-ia` which improves support but
works best with the Intel. In any case, I've added some compiler flags to the CMake configuration to enable debugging
```CMake
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g")
  # set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fprofile-arcs -ftest-coverage")
endif()
```

This is an area open to future work.

### Linking Fortran and C

Linking Fortran and C requires using the `iso_c_binding` in Fortran code and paying careful attention to data types. For this demo,
I added a few new targets to the project so that now it can be built with any combination of mixed languages:

- **newtonraphson_fortran**: The `newtonraphson` iteration library written in **Fortran**
- **newtonraphson_c**: The `newtonraphson` iteration library written in **C**
- **sqrt_fortran_c**: The `sqrt` main code writting in **Fortran** using the **C** version of `newtonraphson`
- **sqrt_c_fortran**: The `sqrt` main code writting in **C** using the **Fortran** version of `newtonraphson`
- **sqrt_c**: The `sqrt` main code writting in **C** using the **C** version of `newtonraphson`
- **sqrt_fortran**: The `sqrt` main code writting in **Fortran** using the **Fortran** version of `newtonraphson`

The fortran-c-binding can be seen in `newtonraphson.f90`; for example:
```Fortran
real(c_double) function nr_sqrt(n, x0, iterations, printIts) bind(c, name='nr_sqrt')
```

Further, the `sqrt_c.f90` program defines an interface for communication with c:
```Fortran
interface
    integer(c_int) function passarrays (in, out) bind (c)
      use iso_c_binding
      implicit none
      real(c_double), intent(in) :: in(4)
      real(c_double), intent(out) :: out(4)
    end function
    function initializer () bind (c)
      use iso_c_binding
      integer(c_int) :: initializer
    end function
    function nr_sqrt ( input, x0, iterations, printIts ) bind (c)
      use iso_c_binding
      real(c_double), value :: input
      real(c_double), value :: x0
      integer(c_int), value :: iterations
      logical(c_bool), value :: printIts
      real(c_double) :: nr_sqrt
    end function nr_sqrt
end interface
```

As a follow on demo to simply linking libraries of mixed languages, I added a demo of passing arrays between the
two as this is not always trivial.

C code:
```C++
int passarrays(double in[4], double out[4])
{
  memcpy(out, in, 4 * sizeof(double));
  return 0;
}
```

Fortran code:
```Fortran
real(C_DOUBLE), dimension(0:3) :: in, out

! passing arrays to c and back
in = (/1,2,3,4/)
print *, "uninitialized out: ", out
result_int = passarrays(in, out)
print *, "after c out: ", out
```

### Continuous integration with TravisCI

[![Build Status](https://travis-ci.org/rafmudaf/sqrt.svg?branch=master)](https://travis-ci.org/rafmudaf/sqrt)

Continuous integration is configured here with [TravisCI](https://travis-ci.org/rafmudaf/sqrt). It is currently configured to run
on a macOS 10.12 (High Sierra) image with GCC version 7. The continuous integration system compiles the main program, compiles the unit
tests, and runs the unit tests.

### Continuous integration with GitHub Actions

TravisCI is free and well supported, but it does have its downsides. Namely, the build process can take some time
to get started, long builds and tests time out, and the underlying images dont seem to be completely stable. When
GitHub announced an alternative with [GitHub Actions](https://github.com/features/actions), I was pretty excited at the
possibility of incorporating testing alongside the code repository.

To demo this feature, I build a Dockerfile and configured a GitHub workflow:

```
workflow "New workflow" {
  on = "push"
  resolves = ["sqrt.pytest"]
}

action "sqrt.build" {
  uses = "actions/docker/cli@master"
  args = "build -t sqrt-$GITHUB_SHA:latest ."
}

action "sqrt.pytest" {
  uses = "actions/docker/cli@master"
  needs = ["sqrt.build"]
  args = "run sqrt-$GITHUB_SHA:latest pytest"
}
```

This entire workflow builds on the standard Ubuntu Docker image, but the power here is that you can
start from a prebuilt Docker image so that recompiling and running tests can be made more effective
and efficient with your own custom environment. You can start with a Docker image from your latest release
and only rebuild what has changed.

Finally, with this close integration to GitHub, you can easily create new releases any time you merge
into `master`.
