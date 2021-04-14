
import sys
import numpy as np
from ctypes import CDLL, POINTER, c_int, c_double

if sys.platform == "darwin":
    library_extension = "dylib"
elif sys.platform == "linux":
    library_extension = "so"
elif sys.platform == "win32":
    library_extension = "dll"
# elif sys.platform == "cygwin":
# ??
#     library_extension = "dll"

newtonraphsonlib = CDLL('./build/libnewtonraphson_fortran.{}'.format(library_extension))

def test_int_2x():
    desired = 8
    actual = newtonraphsonlib.int_2x(4)
    np.testing.assert_almost_equal(actual, desired)


def test_int_sq():
    desired = 16
    actual = newtonraphsonlib.int_sq(4)
    np.testing.assert_almost_equal(actual, desired)

if __name__=="__main__":
    test_int_2x()
    test_int_sq()
    newtonraphsonlib.randn()
