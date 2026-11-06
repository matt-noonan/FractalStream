"""Write small FractalStream 1.0 save files, for LegacyBinarySpec.

They have the two layouts real save files do (see Language.Legacy.Binary),
but their own short scripts. Run with: python3 make-fixtures.py
"""
import plistlib
from plistlib import UID

def classes(name, *supers):
    return {'$classes': [name, *supers, 'NSObject'], '$classname': name}

def archive(objects):
    return plistlib.dumps({'$archiver': 'NSKeyedArchiver', '$version': 100000,
                           '$top': {'root': UID(1)}, '$objects': objects},
                          fmt=plistlib.FMT_BINARY)

# The usual layout: {type: "editor", editor: [title, script, rtfd, ...]},
# with a title that isn't ASCII (so it's stored as UTF-16).
editor = archive([
    '$null',
    {'type': UID(2), 'editor': UID(3), '$class': UID(9)},
    'editor',
    {'NS.objects': [UID(4), UID(5), UID(7), b'\x00\x01'], '$class': UID(8)},
    'Julia set for z² − 1',
    {'NS.string': 'iterate z^2 - 1 until z escapes.\n', '$class': UID(6)},
    classes('NSMutableString', 'NSString'),
    b'rtfd\x00\x00',
    classes('NSMutableArray', 'NSArray'),
    classes('FSSave'),
])

# The oldest layout: {$0: "editor", $1: [title, script, rtfd]}, with old
# Mac OS line endings and an `end.` that ends no block.
oldest = archive([
    '$null',
    {'$0': UID(2), '$1': UID(3), '$class': UID(9)},
    'editor',
    {'NS.objects': [UID(4), UID(5), UID(7)], '$class': UID(8)},
    'Oldest',
    {'NS.string': 'set w to z.\riterate w^2 + c on w until w escapes.\rif w escapes then [Out].\rend.', '$class': UID(6)},
    classes('NSMutableString', 'NSString'),
    b'rtfd\x00\x00',
    classes('NSMutableArray', 'NSArray'),
    classes('FSSave'),
])

open('editor-layout.fs', 'wb').write(editor)
open('oldest-layout.fs', 'wb').write(oldest)
