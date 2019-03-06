"""Low-level http related exceptions."""


from typing import Optional, Union

from .typedefs import _CIMultiDict

__all__ = ('HttpProcessingError',)


class HttpProcessingError(Exception):
    """HTTP error.

    Shortcut for raising HTTP errors with custom code, message and headers.

    code: HTTP Error code.
    message: (optional) Error message.
    headers: (optional) Headers to be sent in response, a list of pairs
    """

    code = 0
    message = ''
    headers = None

    def __init__(self, *,
                 code: Optional[int]=None,
                 message: str='',
                 headers: Optional[_CIMultiDict]=None) -> None:
        if code is not None:
            self.code = code
        self.headers = headers
        self.message = message

        super().__init__("%s, message='%s'" % (self.code, message))


class BadHttpMessage(HttpProcessingError):

    code = 400
    message = 'Bad Request'

    def __init__(self, message: str, *,
                 headers: Optional[_CIMultiDict]=None) -> None:
        super().__init__(message=message, headers=headers)


class HttpBadRequest(BadHttpMessage):

    code = 400
    message = 'Bad Request'


class PayloadEncodingError(BadHttpMessage):
    """Base class for payload errors"""


class ContentEncodingError(PayloadEncodingError):
    """Content encoding error."""


class TransferEncodingError(PayloadEncodingError):
    """transfer encoding error."""


class ContentLengthError(PayloadEncodingError):
    """Not enough data for satisfy content length header."""


class LineTooLong(BadHttpMessage):

    def __init__(self, line: str,
                 limit: str='Unknown',
                 actual_size: str='Unknown') -> None:
        super().__init__(
            "Got more than %s bytes (%s) when reading %s." % (
                limit, actual_size, line))


class InvalidHeader(BadHttpMessage):

    def __init__(self, hdr: Union[bytes, str]) -> None:
        if isinstance(hdr, bytes):
            hdr = hdr.decode('utf-8', 'surrogateescape')
        super().__init__('Invalid HTTP Header: {}'.format(hdr))
        self.hdr = hdr


class BadStatusLine(BadHttpMessage):

    def __init__(self, line: str='') -> None:
        if not line:
            line = repr(line)
        self.args = line,
        self.line = line


class InvalidURLError(BadHttpMessage):
    pass
