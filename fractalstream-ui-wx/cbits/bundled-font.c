/* Make a font, given its file's bytes, usable by name in this process
   only (see UI.BundledFont): with GDI on Windows (which Scintilla draws
   with) and with Core Text on macOS. Elsewhere there's nothing to do:
   fontconfig finds the system's fonts, and DejaVu is usually one of them.
   Returns nonzero if the font was added. */
#include <stddef.h>

#if defined(_WIN32)
#include <windows.h>

int fs_add_bundled_font(const void *bytes, size_t length)
{
  DWORD count = 0;
  /* (GDI copies the bytes) */
  HANDLE h = AddFontMemResourceEx((PVOID) bytes, (DWORD) length, NULL, &count);
  return h != NULL && count > 0;
}

#elif defined(__APPLE__)
#include <CoreFoundation/CoreFoundation.h>
#include <CoreGraphics/CoreGraphics.h>
#include <CoreText/CoreText.h>

int fs_add_bundled_font(const void *bytes, size_t length)
{
  CFDataRef data = CFDataCreate(NULL, (const UInt8 *) bytes, (CFIndex) length);
  if (!data) return 0;
  CGDataProviderRef provider = CGDataProviderCreateWithCFData(data);
  CFRelease(data);
  if (!provider) return 0;
  CGFontRef font = CGFontCreateWithDataProvider(provider);
  CGDataProviderRelease(provider);
  if (!font) return 0;
  CFErrorRef error = NULL;
  /* (deprecated in macOS 15, in favor of registering a file: this one
     needs none, and still works) */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  bool ok = CTFontManagerRegisterGraphicsFont(font, &error);
#pragma clang diagnostic pop
  if (error) {
    /* (already registered, as when the user has it installed, is fine) */
    if (CFErrorGetCode(error) == kCTFontManagerErrorAlreadyRegistered) ok = true;
    CFRelease(error);
  }
  CGFontRelease(font);
  return ok;
}

#else

int fs_add_bundled_font(const void *bytes, size_t length)
{
  (void) bytes; (void) length;
  return 1;
}

#endif
