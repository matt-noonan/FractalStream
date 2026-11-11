/* The math functions the wasm backend's kernels import (module "math"; see
   Backend.Wasm.Gen.mathImports), from the C library (wasi-libc's, which is
   musl's), so that every host computes them alike. dev-tools/wasm/build.sh
   compiles this to fractalstream-backend-wasm/js/libm.wasm.

   Kernels used to import JavaScript's Math functions instead, but calling
   JavaScript from wasm on several worker threads at once made Node (V8) on
   macOS/ARM crash now and then (see WEB_PLAN.md). */
#include <math.h>

#define EXPORT(name) __attribute__((export_name(#name)))

EXPORT(sin)   double fs_sin(double x)   { return sin(x); }
EXPORT(cos)   double fs_cos(double x)   { return cos(x); }
EXPORT(tan)   double fs_tan(double x)   { return tan(x); }
EXPORT(sinh)  double fs_sinh(double x)  { return sinh(x); }
EXPORT(cosh)  double fs_cosh(double x)  { return cosh(x); }
EXPORT(tanh)  double fs_tanh(double x)  { return tanh(x); }
EXPORT(asin)  double fs_asin(double x)  { return asin(x); }
EXPORT(acos)  double fs_acos(double x)  { return acos(x); }
EXPORT(atan)  double fs_atan(double x)  { return atan(x); }
EXPORT(asinh) double fs_asinh(double x) { return asinh(x); }
EXPORT(acosh) double fs_acosh(double x) { return acosh(x); }
EXPORT(atanh) double fs_atanh(double x) { return atanh(x); }
EXPORT(exp)   double fs_exp(double x)   { return exp(x); }
EXPORT(log)   double fs_log(double x)   { return log(x); }
EXPORT(pow)   double fs_pow(double x, double y)   { return pow(x, y); }
EXPORT(atan2) double fs_atan2(double y, double x) { return atan2(y, x); }
EXPORT(hypot) double fs_hypot(double x, double y) { return hypot(x, y); }
EXPORT(fmod)  double fs_fmod(double x, double y)  { return fmod(x, y); }
