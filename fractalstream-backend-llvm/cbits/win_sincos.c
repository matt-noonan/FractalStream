/* LLVM merges sin(x) and cos(x) of the same x into one call to sincos, a
 * GNU extension that the Windows C runtime doesn't provide.  The JIT defines
 * sincos as this function on Windows (see defineRuntimeSymbols in
 * Backend/LLVM.hs).
 */
#include <math.h>

void fractalstream_sincos(double x, double *s, double *c)
{
  *s = sin(x);
  *c = cos(x);
}
