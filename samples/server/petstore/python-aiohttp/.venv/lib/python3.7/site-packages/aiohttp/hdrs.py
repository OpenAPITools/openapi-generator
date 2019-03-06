"""HTTP Headers constants."""

# After changing the file content call ./tools/gen.py
# to regenerate the headers parser

from multidict import istr

METH_ANY = '*'
METH_CONNECT = 'CONNECT'
METH_HEAD = 'HEAD'
METH_GET = 'GET'
METH_DELETE = 'DELETE'
METH_OPTIONS = 'OPTIONS'
METH_PATCH = 'PATCH'
METH_POST = 'POST'
METH_PUT = 'PUT'
METH_TRACE = 'TRACE'

METH_ALL = {METH_CONNECT, METH_HEAD, METH_GET, METH_DELETE,
            METH_OPTIONS, METH_PATCH, METH_POST, METH_PUT, METH_TRACE}


ACCEPT = istr('Accept')
ACCEPT_CHARSET = istr('Accept-Charset')
ACCEPT_ENCODING = istr('Accept-Encoding')
ACCEPT_LANGUAGE = istr('Accept-Language')
ACCEPT_RANGES = istr('Accept-Ranges')
ACCESS_CONTROL_MAX_AGE = istr('Access-Control-Max-Age')
ACCESS_CONTROL_ALLOW_CREDENTIALS = istr('Access-Control-Allow-Credentials')
ACCESS_CONTROL_ALLOW_HEADERS = istr('Access-Control-Allow-Headers')
ACCESS_CONTROL_ALLOW_METHODS = istr('Access-Control-Allow-Methods')
ACCESS_CONTROL_ALLOW_ORIGIN = istr('Access-Control-Allow-Origin')
ACCESS_CONTROL_EXPOSE_HEADERS = istr('Access-Control-Expose-Headers')
ACCESS_CONTROL_REQUEST_HEADERS = istr('Access-Control-Request-Headers')
ACCESS_CONTROL_REQUEST_METHOD = istr('Access-Control-Request-Method')
AGE = istr('Age')
ALLOW = istr('Allow')
AUTHORIZATION = istr('Authorization')
CACHE_CONTROL = istr('Cache-Control')
CONNECTION = istr('Connection')
CONTENT_DISPOSITION = istr('Content-Disposition')
CONTENT_ENCODING = istr('Content-Encoding')
CONTENT_LANGUAGE = istr('Content-Language')
CONTENT_LENGTH = istr('Content-Length')
CONTENT_LOCATION = istr('Content-Location')
CONTENT_MD5 = istr('Content-MD5')
CONTENT_RANGE = istr('Content-Range')
CONTENT_TRANSFER_ENCODING = istr('Content-Transfer-Encoding')
CONTENT_TYPE = istr('Content-Type')
COOKIE = istr('Cookie')
DATE = istr('Date')
DESTINATION = istr('Destination')
DIGEST = istr('Digest')
ETAG = istr('Etag')
EXPECT = istr('Expect')
EXPIRES = istr('Expires')
FORWARDED = istr('Forwarded')
FROM = istr('From')
HOST = istr('Host')
IF_MATCH = istr('If-Match')
IF_MODIFIED_SINCE = istr('If-Modified-Since')
IF_NONE_MATCH = istr('If-None-Match')
IF_RANGE = istr('If-Range')
IF_UNMODIFIED_SINCE = istr('If-Unmodified-Since')
KEEP_ALIVE = istr('Keep-Alive')
LAST_EVENT_ID = istr('Last-Event-ID')
LAST_MODIFIED = istr('Last-Modified')
LINK = istr('Link')
LOCATION = istr('Location')
MAX_FORWARDS = istr('Max-Forwards')
ORIGIN = istr('Origin')
PRAGMA = istr('Pragma')
PROXY_AUTHENTICATE = istr('Proxy-Authenticate')
PROXY_AUTHORIZATION = istr('Proxy-Authorization')
RANGE = istr('Range')
REFERER = istr('Referer')
RETRY_AFTER = istr('Retry-After')
SEC_WEBSOCKET_ACCEPT = istr('Sec-WebSocket-Accept')
SEC_WEBSOCKET_VERSION = istr('Sec-WebSocket-Version')
SEC_WEBSOCKET_PROTOCOL = istr('Sec-WebSocket-Protocol')
SEC_WEBSOCKET_EXTENSIONS = istr('Sec-WebSocket-Extensions')
SEC_WEBSOCKET_KEY = istr('Sec-WebSocket-Key')
SEC_WEBSOCKET_KEY1 = istr('Sec-WebSocket-Key1')
SERVER = istr('Server')
SET_COOKIE = istr('Set-Cookie')
TE = istr('TE')
TRAILER = istr('Trailer')
TRANSFER_ENCODING = istr('Transfer-Encoding')
UPGRADE = istr('Upgrade')
WEBSOCKET = istr('WebSocket')
URI = istr('URI')
USER_AGENT = istr('User-Agent')
VARY = istr('Vary')
VIA = istr('Via')
WANT_DIGEST = istr('Want-Digest')
WARNING = istr('Warning')
WWW_AUTHENTICATE = istr('WWW-Authenticate')
X_FORWARDED_FOR = istr('X-Forwarded-For')
X_FORWARDED_HOST = istr('X-Forwarded-Host')
X_FORWARDED_PROTO = istr('X-Forwarded-Proto')
