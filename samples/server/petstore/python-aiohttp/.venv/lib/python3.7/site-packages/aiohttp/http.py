import http.server
import sys
from typing import Mapping, Tuple  # noqa

from . import __version__
from .http_exceptions import HttpProcessingError
from .http_parser import (
    HeadersParser,
    HttpParser,
    HttpRequestParser,
    HttpResponseParser,
    RawRequestMessage,
    RawResponseMessage,
)
from .http_websocket import (
    WS_CLOSED_MESSAGE,
    WS_CLOSING_MESSAGE,
    WS_KEY,
    WebSocketError,
    WebSocketReader,
    WebSocketWriter,
    WSCloseCode,
    WSMessage,
    WSMsgType,
    ws_ext_gen,
    ws_ext_parse,
)
from .http_writer import (
    HttpVersion,
    HttpVersion10,
    HttpVersion11,
    StreamWriter,
)

__all__ = (
    'HttpProcessingError', 'RESPONSES', 'SERVER_SOFTWARE',

    # .http_writer
    'StreamWriter', 'HttpVersion', 'HttpVersion10', 'HttpVersion11',

    # .http_parser
    'HeadersParser', 'HttpParser',
    'HttpRequestParser', 'HttpResponseParser',
    'RawRequestMessage', 'RawResponseMessage',

    # .http_websocket
    'WS_CLOSED_MESSAGE', 'WS_CLOSING_MESSAGE', 'WS_KEY',
    'WebSocketReader', 'WebSocketWriter', 'ws_ext_gen', 'ws_ext_parse',
    'WSMessage', 'WebSocketError', 'WSMsgType', 'WSCloseCode',
)


SERVER_SOFTWARE = 'Python/{0[0]}.{0[1]} aiohttp/{1}'.format(
    sys.version_info, __version__)  # type: str

RESPONSES = http.server.BaseHTTPRequestHandler.responses  # type: Mapping[int, Tuple[str, str]]  # noqa
