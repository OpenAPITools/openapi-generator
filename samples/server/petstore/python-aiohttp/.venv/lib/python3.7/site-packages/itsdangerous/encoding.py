import base64
import string
import struct

from ._compat import text_type
from .exc import BadData


def want_bytes(s, encoding="utf-8", errors="strict"):
    if isinstance(s, text_type):
        s = s.encode(encoding, errors)
    return s


def base64_encode(string):
    """Base64 encode a string of bytes or text. The resulting bytes are
    safe to use in URLs.
    """
    string = want_bytes(string)
    return base64.urlsafe_b64encode(string).rstrip(b"=")


def base64_decode(string):
    """Base64 decode a URL-safe string of bytes or text. The result is
    bytes.
    """
    string = want_bytes(string, encoding="ascii", errors="ignore")
    string += b"=" * (-len(string) % 4)

    try:
        return base64.urlsafe_b64decode(string)
    except (TypeError, ValueError):
        raise BadData("Invalid base64-encoded data")


# The alphabet used by base64.urlsafe_*
_base64_alphabet = (string.ascii_letters + string.digits + "-_=").encode("ascii")

_int64_struct = struct.Struct(">Q")
_int_to_bytes = _int64_struct.pack
_bytes_to_int = _int64_struct.unpack


def int_to_bytes(num):
    return _int_to_bytes(num).lstrip(b"\x00")


def bytes_to_int(bytestr):
    return _bytes_to_int(bytestr.rjust(8, b"\x00"))[0]
