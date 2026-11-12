// Trackpad pinches ("magnify" gestures), which wxWidgets has (as
// wxEVT_MAGNIFY mouse events, with their magnification) but wxHaskell
// doesn't bind. See UI.Magnify.
#include <wx/wx.h>

extern "C" {

// What to do with each pinch event: its magnification (the change in
// scale since the last event: positive to zoom in), and the pointer's
// position in the window. (wxWidgets 3.2's magnify events don't say
// where in the gesture they are: a pinch is over when they stop.)
typedef void (*fs_magnify_callback)(double magnification, int x, int y);

// Call `callback` for each pinch event on `window` (a wxWindow).
void fs_on_magnify(void *window, fs_magnify_callback callback) {
  wxWindow *w = static_cast<wxWindow *>(window);
  w->Bind(wxEVT_MAGNIFY, [callback](wxMouseEvent &e) {
    wxPoint p = e.GetPosition();
    callback(e.GetMagnification(), p.x, p.y);
  });
}

}
