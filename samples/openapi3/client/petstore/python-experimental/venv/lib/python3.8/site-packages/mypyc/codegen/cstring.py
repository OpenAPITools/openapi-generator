"""Encode valid C string literals from Python strings.

If a character is not allowed in C string literals, it is either emitted
as a simple escape sequence (e.g. '\\n'), or an octal escape sequence
with exactly three digits ('\\oXXX'). Question marks are escaped to
prevent trigraphs in the string literal from being interpreted. Note
that '\\?' is an invalid escape sequence in Python.

Consider the string literal "AB\\xCDEF". As one would expect, Python
parses it as ['A', 'B', 0xCD, 'E', 'F']. However, the C standard
specifies that all hexadecimal digits immediately following '\\x' will
be interpreted as part of the escape sequence. Therefore, it is
unexpectedly parsed as ['A', 'B', 0xCDEF].

Emitting ("AB\\xCD" "EF") would avoid this behaviour. However, we opt
for simplicity and use octal escape sequences instead. They do not
suffer from the same issue as they are defined to parse at most three
octal digits.
"""

import string
from typing import Tuple

CHAR_MAP = ['\\{:03o}'.format(i) for i in range(256)]

# It is safe to use string.printable as it always uses the C locale.
for c in string.printable:
    CHAR_MAP[ord(c)] = c

# These assignments must come last because we prioritize simple escape
# sequences over any other representation.
for c in ('\'', '"', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v'):
    escaped = '\\{}'.format(c)
    decoded = escaped.encode('ascii').decode('unicode_escape')
    CHAR_MAP[ord(decoded)] = escaped

# This escape sequence is invalid in Python.
CHAR_MAP[ord('?')] = r'\?'


def encode_as_c_string(s: str) -> Tuple[str, int]:
    """Produce a quoted C string literal and its size, for a UTF-8 string."""
    return encode_bytes_as_c_string(s.encode('utf-8'))


def encode_bytes_as_c_string(b: bytes) -> Tuple[str, int]:
    """Produce a quoted C string literal and its size, for a byte string."""
    escaped = ''.join([CHAR_MAP[i] for i in b])
    return '"{}"'.format(escaped), len(b)
