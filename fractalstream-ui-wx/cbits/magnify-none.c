/* Where wxWidgets comes as a prebuilt DLL (Windows), with no headers to
   compile cbits/magnify.cpp against: no pinches. See UI.Magnify. */
typedef void (*fs_magnify_callback)(double magnification, int x, int y);
void fs_on_magnify(void *window, fs_magnify_callback callback) { (void) window; (void) callback; }
