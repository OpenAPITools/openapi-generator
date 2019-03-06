import asyncio
import functools
import random
import sys
import traceback
import warnings
from collections import defaultdict, deque
from contextlib import suppress
from http.cookies import SimpleCookie
from itertools import cycle, islice
from time import monotonic
from types import TracebackType
from typing import (  # noqa
    TYPE_CHECKING,
    Any,
    Awaitable,
    Callable,
    DefaultDict,
    Dict,
    Iterator,
    List,
    Optional,
    Set,
    Tuple,
    Type,
    Union,
    cast,
)

import attr

from . import hdrs, helpers
from .abc import AbstractResolver
from .client_exceptions import (
    ClientConnectionError,
    ClientConnectorCertificateError,
    ClientConnectorError,
    ClientConnectorSSLError,
    ClientHttpProxyError,
    ClientProxyConnectionError,
    ServerFingerprintMismatch,
    cert_errors,
    ssl_errors,
)
from .client_proto import ResponseHandler
from .client_reqrep import ClientRequest, Fingerprint, _merge_ssl_params
from .helpers import (
    PY_36,
    CeilTimeout,
    get_running_loop,
    is_ip_address,
    noop2,
    sentinel,
)
from .http import RESPONSES
from .locks import EventResultOrError
from .resolver import DefaultResolver

try:
    import ssl
    SSLContext = ssl.SSLContext
except ImportError:  # pragma: no cover
    ssl = None  # type: ignore
    SSLContext = object  # type: ignore


__all__ = ('BaseConnector', 'TCPConnector', 'UnixConnector')


if TYPE_CHECKING:  # pragma: no cover
    from .client import ClientTimeout  # noqa
    from .client_reqrep import ConnectionKey  # noqa
    from .tracing import Trace  # noqa


class _DeprecationWaiter:
    __slots__ = ('_awaitable', '_awaited')

    def __init__(self, awaitable: Awaitable[Any]) -> None:
        self._awaitable = awaitable
        self._awaited = False

    def __await__(self) -> Any:
        self._awaited = True
        return self._awaitable.__await__()

    def __del__(self) -> None:
        if not self._awaited:
            warnings.warn("Connector.close() is a coroutine, "
                          "please use await connector.close()",
                          DeprecationWarning)


class Connection:

    _source_traceback = None
    _transport = None

    def __init__(self, connector: 'BaseConnector',
                 key: 'ConnectionKey',
                 protocol: ResponseHandler,
                 loop: asyncio.AbstractEventLoop) -> None:
        self._key = key
        self._connector = connector
        self._loop = loop
        self._protocol = protocol  # type: Optional[ResponseHandler]
        self._callbacks = []  # type: List[Callable[[], None]]

        if loop.get_debug():
            self._source_traceback = traceback.extract_stack(sys._getframe(1))

    def __repr__(self) -> str:
        return 'Connection<{}>'.format(self._key)

    def __del__(self, _warnings: Any=warnings) -> None:
        if self._protocol is not None:
            if PY_36:
                kwargs = {'source': self}
            else:
                kwargs = {}
            _warnings.warn('Unclosed connection {!r}'.format(self),
                           ResourceWarning,
                           **kwargs)
            if self._loop.is_closed():
                return

            self._connector._release(
                self._key, self._protocol, should_close=True)

            context = {'client_connection': self,
                       'message': 'Unclosed connection'}
            if self._source_traceback is not None:
                context['source_traceback'] = self._source_traceback
            self._loop.call_exception_handler(context)

    @property
    def loop(self) -> asyncio.AbstractEventLoop:
        warnings.warn("connector.loop property is deprecated",
                      DeprecationWarning,
                      stacklevel=2)
        return self._loop

    @property
    def transport(self) -> Optional[asyncio.Transport]:
        if self._protocol is None:
            return None
        return self._protocol.transport

    @property
    def protocol(self) -> Optional[ResponseHandler]:
        return self._protocol

    def add_callback(self, callback: Callable[[], None]) -> None:
        if callback is not None:
            self._callbacks.append(callback)

    def _notify_release(self) -> None:
        callbacks, self._callbacks = self._callbacks[:], []

        for cb in callbacks:
            with suppress(Exception):
                cb()

    def close(self) -> None:
        self._notify_release()

        if self._protocol is not None:
            self._connector._release(
                self._key, self._protocol, should_close=True)
            self._protocol = None

    def release(self) -> None:
        self._notify_release()

        if self._protocol is not None:
            self._connector._release(
                self._key, self._protocol,
                should_close=self._protocol.should_close)
            self._protocol = None

    @property
    def closed(self) -> bool:
        return self._protocol is None or not self._protocol.is_connected()


class _TransportPlaceholder:
    """ placeholder for BaseConnector.connect function """

    def close(self) -> None:
        pass


class BaseConnector:
    """Base connector class.

    keepalive_timeout - (optional) Keep-alive timeout.
    force_close - Set to True to force close and do reconnect
        after each request (and between redirects).
    limit - The total number of simultaneous connections.
    limit_per_host - Number of simultaneous connections to one host.
    enable_cleanup_closed - Enables clean-up closed ssl transports.
                            Disabled by default.
    loop - Optional event loop.
    """

    _closed = True  # prevent AttributeError in __del__ if ctor was failed
    _source_traceback = None

    # abort transport after 2 seconds (cleanup broken connections)
    _cleanup_closed_period = 2.0

    def __init__(self, *,
                 keepalive_timeout: Union[object, None, float]=sentinel,
                 force_close: bool=False,
                 limit: int=100, limit_per_host: int=0,
                 enable_cleanup_closed: bool=False,
                 loop: Optional[asyncio.AbstractEventLoop]=None) -> None:

        if force_close:
            if keepalive_timeout is not None and \
               keepalive_timeout is not sentinel:
                raise ValueError('keepalive_timeout cannot '
                                 'be set if force_close is True')
        else:
            if keepalive_timeout is sentinel:
                keepalive_timeout = 15.0

        loop = get_running_loop(loop)

        self._closed = False
        if loop.get_debug():
            self._source_traceback = traceback.extract_stack(sys._getframe(1))

        self._conns = {}  # type: Dict[ConnectionKey, List[Tuple[ResponseHandler, float]]]  # noqa
        self._limit = limit
        self._limit_per_host = limit_per_host
        self._acquired = set()  # type: Set[ResponseHandler]
        self._acquired_per_host = defaultdict(set)  # type: DefaultDict[ConnectionKey, Set[ResponseHandler]]  # noqa
        self._keepalive_timeout = cast(float, keepalive_timeout)
        self._force_close = force_close

        # {host_key: FIFO list of waiters}
        self._waiters = defaultdict(deque)  # type: ignore

        self._loop = loop
        self._factory = functools.partial(ResponseHandler, loop=loop)

        self.cookies = SimpleCookie()

        # start keep-alive connection cleanup task
        self._cleanup_handle = None

        # start cleanup closed transports task
        self._cleanup_closed_handle = None
        self._cleanup_closed_disabled = not enable_cleanup_closed
        self._cleanup_closed_transports = []  # type: List[Optional[asyncio.Transport]]  # noqa
        self._cleanup_closed()

    def __del__(self, _warnings: Any=warnings) -> None:
        if self._closed:
            return
        if not self._conns:
            return

        conns = [repr(c) for c in self._conns.values()]

        self._close()

        if PY_36:
            kwargs = {'source': self}
        else:
            kwargs = {}
        _warnings.warn("Unclosed connector {!r}".format(self),
                       ResourceWarning,
                       **kwargs)
        context = {'connector': self,
                   'connections': conns,
                   'message': 'Unclosed connector'}
        if self._source_traceback is not None:
            context['source_traceback'] = self._source_traceback
        self._loop.call_exception_handler(context)

    def __enter__(self) -> 'BaseConnector':
        warnings.warn('"witn Connector():" is deprecated, '
                      'use "async with Connector():" instead',
                      DeprecationWarning)
        return self

    def __exit__(self, *exc: Any) -> None:
        self.close()

    async def __aenter__(self) -> 'BaseConnector':
        return self

    async def __aexit__(self,
                        exc_type: Optional[Type[BaseException]]=None,
                        exc_value: Optional[BaseException]=None,
                        exc_traceback: Optional[TracebackType]=None
                        ) -> None:
        await self.close()

    @property
    def force_close(self) -> bool:
        """Ultimately close connection on releasing if True."""
        return self._force_close

    @property
    def limit(self) -> int:
        """The total number for simultaneous connections.

        If limit is 0 the connector has no limit.
        The default limit size is 100.
        """
        return self._limit

    @property
    def limit_per_host(self) -> int:
        """The limit_per_host for simultaneous connections
        to the same endpoint.

        Endpoints are the same if they are have equal
        (host, port, is_ssl) triple.

        """
        return self._limit_per_host

    def _cleanup(self) -> None:
        """Cleanup unused transports."""
        if self._cleanup_handle:
            self._cleanup_handle.cancel()

        now = self._loop.time()
        timeout = self._keepalive_timeout

        if self._conns:
            connections = {}
            deadline = now - timeout
            for key, conns in self._conns.items():
                alive = []
                for proto, use_time in conns:
                    if proto.is_connected():
                        if use_time - deadline < 0:
                            transport = proto.transport
                            proto.close()
                            if (key.is_ssl and
                                    not self._cleanup_closed_disabled):
                                self._cleanup_closed_transports.append(
                                    transport)
                        else:
                            alive.append((proto, use_time))

                if alive:
                    connections[key] = alive

            self._conns = connections

        if self._conns:
            self._cleanup_handle = helpers.weakref_handle(
                self, '_cleanup', timeout, self._loop)

    def _drop_acquired_per_host(self, key: 'ConnectionKey',
                                val: ResponseHandler) -> None:
        acquired_per_host = self._acquired_per_host
        if key not in acquired_per_host:
            return
        conns = acquired_per_host[key]
        conns.remove(val)
        if not conns:
            del self._acquired_per_host[key]

    def _cleanup_closed(self) -> None:
        """Double confirmation for transport close.
        Some broken ssl servers may leave socket open without proper close.
        """
        if self._cleanup_closed_handle:
            self._cleanup_closed_handle.cancel()

        for transport in self._cleanup_closed_transports:
            if transport is not None:
                transport.abort()

        self._cleanup_closed_transports = []

        if not self._cleanup_closed_disabled:
            self._cleanup_closed_handle = helpers.weakref_handle(
                self, '_cleanup_closed',
                self._cleanup_closed_period, self._loop)

    def close(self) -> Awaitable[None]:
        """Close all opened transports."""
        self._close()
        return _DeprecationWaiter(noop2())

    def _close(self) -> None:
        if self._closed:
            return

        self._closed = True

        try:
            if self._loop.is_closed():
                return

            # cancel cleanup task
            if self._cleanup_handle:
                self._cleanup_handle.cancel()

            # cancel cleanup close task
            if self._cleanup_closed_handle:
                self._cleanup_closed_handle.cancel()

            for data in self._conns.values():
                for proto, t0 in data:
                    proto.close()

            for proto in self._acquired:
                proto.close()

            for transport in self._cleanup_closed_transports:
                if transport is not None:
                    transport.abort()

        finally:
            self._conns.clear()
            self._acquired.clear()
            self._waiters.clear()
            self._cleanup_handle = None
            self._cleanup_closed_transports.clear()
            self._cleanup_closed_handle = None

    @property
    def closed(self) -> bool:
        """Is connector closed.

        A readonly property.
        """
        return self._closed

    def _available_connections(self, key: 'ConnectionKey') -> int:
        """
        Return number of available connections taking into account
        the limit, limit_per_host and the connection key.

        If it returns less than 1 means that there is no connections
        availables.
        """

        if self._limit:
            # total calc available connections
            available = self._limit - len(self._acquired)

            # check limit per host
            if (self._limit_per_host and available > 0 and
                    key in self._acquired_per_host):
                acquired = self._acquired_per_host.get(key)
                assert acquired is not None
                available = self._limit_per_host - len(acquired)

        elif self._limit_per_host and key in self._acquired_per_host:
            # check limit per host
            acquired = self._acquired_per_host.get(key)
            assert acquired is not None
            available = self._limit_per_host - len(acquired)
        else:
            available = 1

        return available

    async def connect(self, req: 'ClientRequest',
                      traces: List['Trace'],
                      timeout: 'ClientTimeout') -> Connection:
        """Get from pool or create new connection."""
        key = req.connection_key
        available = self._available_connections(key)

        # Wait if there are no available connections.
        if available <= 0:
            fut = self._loop.create_future()

            # This connection will now count towards the limit.
            waiters = self._waiters[key]
            waiters.append(fut)

            if traces:
                for trace in traces:
                    await trace.send_connection_queued_start()

            try:
                await fut
            except BaseException as e:
                # remove a waiter even if it was cancelled, normally it's
                #  removed when it's notified
                try:
                    waiters.remove(fut)
                except ValueError:  # fut may no longer be in list
                    pass

                raise e
            finally:
                if not waiters:
                    try:
                        del self._waiters[key]
                    except KeyError:
                        # the key was evicted before.
                        pass

            if traces:
                for trace in traces:
                    await trace.send_connection_queued_end()

        proto = self._get(key)
        if proto is None:
            placeholder = cast(ResponseHandler, _TransportPlaceholder())
            self._acquired.add(placeholder)
            self._acquired_per_host[key].add(placeholder)

            if traces:
                for trace in traces:
                    await trace.send_connection_create_start()

            try:
                proto = await self._create_connection(req, traces, timeout)
                if self._closed:
                    proto.close()
                    raise ClientConnectionError("Connector is closed.")
            except BaseException:
                if not self._closed:
                    self._acquired.remove(placeholder)
                    self._drop_acquired_per_host(key, placeholder)
                    self._release_waiter()
                raise
            else:
                if not self._closed:
                    self._acquired.remove(placeholder)
                    self._drop_acquired_per_host(key, placeholder)

            if traces:
                for trace in traces:
                    await trace.send_connection_create_end()
        else:
            if traces:
                for trace in traces:
                    await trace.send_connection_reuseconn()

        self._acquired.add(proto)
        self._acquired_per_host[key].add(proto)
        return Connection(self, key, proto, self._loop)

    def _get(self, key: 'ConnectionKey') -> Optional[ResponseHandler]:
        try:
            conns = self._conns[key]
        except KeyError:
            return None

        t1 = self._loop.time()
        while conns:
            proto, t0 = conns.pop()
            if proto.is_connected():
                if t1 - t0 > self._keepalive_timeout:
                    transport = proto.transport
                    proto.close()
                    # only for SSL transports
                    if key.is_ssl and not self._cleanup_closed_disabled:
                        self._cleanup_closed_transports.append(transport)
                else:
                    if not conns:
                        # The very last connection was reclaimed: drop the key
                        del self._conns[key]
                    return proto

        # No more connections: drop the key
        del self._conns[key]
        return None

    def _release_waiter(self) -> None:
        """
        Iterates over all waiters till found one that is not finsihed and
        belongs to a host that has available connections.
        """
        if not self._waiters:
            return

        # Having the dict keys ordered this avoids to iterate
        # at the same order at each call.
        queues = list(self._waiters.keys())
        random.shuffle(queues)

        for key in queues:
            if self._available_connections(key) < 1:
                continue

            waiters = self._waiters[key]
            while waiters:
                waiter = waiters.popleft()
                if not waiter.done():
                    waiter.set_result(None)
                    return

    def _release_acquired(self, key: 'ConnectionKey',
                          proto: ResponseHandler) -> None:
        if self._closed:
            # acquired connection is already released on connector closing
            return

        try:
            self._acquired.remove(proto)
            self._drop_acquired_per_host(key, proto)
        except KeyError:  # pragma: no cover
            # this may be result of undetermenistic order of objects
            # finalization due garbage collection.
            pass
        else:
            self._release_waiter()

    def _release(self, key: 'ConnectionKey', protocol: ResponseHandler,
                 *, should_close: bool=False) -> None:
        if self._closed:
            # acquired connection is already released on connector closing
            return

        self._release_acquired(key, protocol)

        if self._force_close:
            should_close = True

        if should_close or protocol.should_close:
            transport = protocol.transport
            protocol.close()

            if key.is_ssl and not self._cleanup_closed_disabled:
                self._cleanup_closed_transports.append(transport)
        else:
            conns = self._conns.get(key)
            if conns is None:
                conns = self._conns[key] = []
            conns.append((protocol, self._loop.time()))

            if self._cleanup_handle is None:
                self._cleanup_handle = helpers.weakref_handle(
                    self, '_cleanup', self._keepalive_timeout, self._loop)

    async def _create_connection(self, req: 'ClientRequest',
                                 traces: List['Trace'],
                                 timeout: 'ClientTimeout') -> ResponseHandler:
        raise NotImplementedError()


class _DNSCacheTable:

    def __init__(self, ttl: Optional[float]=None) -> None:
        self._addrs_rr = {}  # type: Dict[Tuple[str, int], Tuple[Iterator[Dict[str, Any]], int]]  # noqa
        self._timestamps = {}  # type: Dict[Tuple[str, int], float]
        self._ttl = ttl

    def __contains__(self, host: object) -> bool:
        return host in self._addrs_rr

    def add(self, key: Tuple[str, int], addrs: List[Dict[str, Any]]) -> None:
        self._addrs_rr[key] = (cycle(addrs), len(addrs))

        if self._ttl:
            self._timestamps[key] = monotonic()

    def remove(self, key: Tuple[str, int]) -> None:
        self._addrs_rr.pop(key, None)

        if self._ttl:
            self._timestamps.pop(key, None)

    def clear(self) -> None:
        self._addrs_rr.clear()
        self._timestamps.clear()

    def next_addrs(self, key: Tuple[str, int]) -> List[Dict[str, Any]]:
        loop, length = self._addrs_rr[key]
        addrs = list(islice(loop, length))
        # Consume one more element to shift internal state of `cycle`
        next(loop)
        return addrs

    def expired(self, key: Tuple[str, int]) -> bool:
        if self._ttl is None:
            return False

        return self._timestamps[key] + self._ttl < monotonic()


class TCPConnector(BaseConnector):
    """TCP connector.

    verify_ssl - Set to True to check ssl certifications.
    fingerprint - Pass the binary sha256
        digest of the expected certificate in DER format to verify
        that the certificate the server presents matches. See also
        https://en.wikipedia.org/wiki/Transport_Layer_Security#Certificate_pinning
    resolver - Enable DNS lookups and use this
        resolver
    use_dns_cache - Use memory cache for DNS lookups.
    ttl_dns_cache - Max seconds having cached a DNS entry, None forever.
    family - socket address family
    local_addr - local tuple of (host, port) to bind socket to

    keepalive_timeout - (optional) Keep-alive timeout.
    force_close - Set to True to force close and do reconnect
        after each request (and between redirects).
    limit - The total number of simultaneous connections.
    limit_per_host - Number of simultaneous connections to one host.
    enable_cleanup_closed - Enables clean-up closed ssl transports.
                            Disabled by default.
    loop - Optional event loop.
    """

    def __init__(self, *, verify_ssl: bool=True,
                 fingerprint: Optional[bytes]=None,
                 use_dns_cache: bool=True, ttl_dns_cache: int=10,
                 family: int=0,
                 ssl_context: Optional[SSLContext]=None,
                 ssl: Union[None, bool, Fingerprint, SSLContext]=None,
                 local_addr: Optional[str]=None,
                 resolver: Optional[AbstractResolver]=None,
                 keepalive_timeout: Union[None, float, object]=sentinel,
                 force_close: bool=False,
                 limit: int=100, limit_per_host: int=0,
                 enable_cleanup_closed: bool=False,
                 loop: Optional[asyncio.AbstractEventLoop]=None):
        super().__init__(keepalive_timeout=keepalive_timeout,
                         force_close=force_close,
                         limit=limit, limit_per_host=limit_per_host,
                         enable_cleanup_closed=enable_cleanup_closed,
                         loop=loop)

        self._ssl = _merge_ssl_params(ssl, verify_ssl, ssl_context,
                                      fingerprint)
        if resolver is None:
            resolver = DefaultResolver(loop=self._loop)
        self._resolver = resolver

        self._use_dns_cache = use_dns_cache
        self._cached_hosts = _DNSCacheTable(ttl=ttl_dns_cache)
        self._throttle_dns_events = {}  # type: Dict[Tuple[str, int], EventResultOrError]  # noqa
        self._family = family
        self._local_addr = local_addr

    def close(self) -> Awaitable[None]:
        """Close all ongoing DNS calls."""
        for ev in self._throttle_dns_events.values():
            ev.cancel()

        return super().close()

    @property
    def family(self) -> int:
        """Socket family like AF_INET."""
        return self._family

    @property
    def use_dns_cache(self) -> bool:
        """True if local DNS caching is enabled."""
        return self._use_dns_cache

    def clear_dns_cache(self,
                        host: Optional[str]=None,
                        port: Optional[int]=None) -> None:
        """Remove specified host/port or clear all dns local cache."""
        if host is not None and port is not None:
            self._cached_hosts.remove((host, port))
        elif host is not None or port is not None:
            raise ValueError("either both host and port "
                             "or none of them are allowed")
        else:
            self._cached_hosts.clear()

    async def _resolve_host(self,
                            host: str, port: int,
                            traces: Optional[List['Trace']]=None
                            ) -> List[Dict[str, Any]]:
        if is_ip_address(host):
            return [{'hostname': host, 'host': host, 'port': port,
                     'family': self._family, 'proto': 0, 'flags': 0}]

        if not self._use_dns_cache:

            if traces:
                for trace in traces:
                    await trace.send_dns_resolvehost_start(host)

            res = (await self._resolver.resolve(
                host, port, family=self._family))

            if traces:
                for trace in traces:
                    await trace.send_dns_resolvehost_end(host)

            return res

        key = (host, port)

        if (key in self._cached_hosts) and \
                (not self._cached_hosts.expired(key)):

            if traces:
                for trace in traces:
                    await trace.send_dns_cache_hit(host)

            return self._cached_hosts.next_addrs(key)

        if key in self._throttle_dns_events:
            if traces:
                for trace in traces:
                    await trace.send_dns_cache_hit(host)
            await self._throttle_dns_events[key].wait()
        else:
            if traces:
                for trace in traces:
                    await trace.send_dns_cache_miss(host)
            self._throttle_dns_events[key] = \
                EventResultOrError(self._loop)
            try:

                if traces:
                    for trace in traces:
                        await trace.send_dns_resolvehost_start(host)

                addrs = await \
                    self._resolver.resolve(host, port, family=self._family)
                if traces:
                    for trace in traces:
                        await trace.send_dns_resolvehost_end(host)

                self._cached_hosts.add(key, addrs)
                self._throttle_dns_events[key].set()
            except BaseException as e:
                # any DNS exception, independently of the implementation
                # is set for the waiters to raise the same exception.
                self._throttle_dns_events[key].set(exc=e)
                raise
            finally:
                self._throttle_dns_events.pop(key)

        return self._cached_hosts.next_addrs(key)

    async def _create_connection(self, req: 'ClientRequest',
                                 traces: List['Trace'],
                                 timeout: 'ClientTimeout') -> ResponseHandler:
        """Create connection.

        Has same keyword arguments as BaseEventLoop.create_connection.
        """
        if req.proxy:
            _, proto = await self._create_proxy_connection(
                req, traces, timeout)
        else:
            _, proto = await self._create_direct_connection(
                req, traces, timeout)

        return proto

    @staticmethod
    @functools.lru_cache(None)
    def _make_ssl_context(verified: bool) -> SSLContext:
        if verified:
            return ssl.create_default_context()
        else:
            sslcontext = ssl.SSLContext(ssl.PROTOCOL_SSLv23)
            sslcontext.options |= ssl.OP_NO_SSLv2
            sslcontext.options |= ssl.OP_NO_SSLv3
            sslcontext.options |= ssl.OP_NO_COMPRESSION
            sslcontext.set_default_verify_paths()
            return sslcontext

    def _get_ssl_context(self, req: 'ClientRequest') -> Optional[SSLContext]:
        """Logic to get the correct SSL context

        0. if req.ssl is false, return None

        1. if ssl_context is specified in req, use it
        2. if _ssl_context is specified in self, use it
        3. otherwise:
            1. if verify_ssl is not specified in req, use self.ssl_context
               (will generate a default context according to self.verify_ssl)
            2. if verify_ssl is True in req, generate a default SSL context
            3. if verify_ssl is False in req, generate a SSL context that
               won't verify
        """
        if req.is_ssl():
            if ssl is None:  # pragma: no cover
                raise RuntimeError('SSL is not supported.')
            sslcontext = req.ssl
            if isinstance(sslcontext, ssl.SSLContext):
                return sslcontext
            if sslcontext is not None:
                # not verified or fingerprinted
                return self._make_ssl_context(False)
            sslcontext = self._ssl
            if isinstance(sslcontext, ssl.SSLContext):
                return sslcontext
            if sslcontext is not None:
                # not verified or fingerprinted
                return self._make_ssl_context(False)
            return self._make_ssl_context(True)
        else:
            return None

    def _get_fingerprint(self,
                         req: 'ClientRequest') -> Optional['Fingerprint']:
        ret = req.ssl
        if isinstance(ret, Fingerprint):
            return ret
        ret = self._ssl
        if isinstance(ret, Fingerprint):
            return ret
        return None

    async def _wrap_create_connection(
            self, *args: Any,
            req: 'ClientRequest',
            timeout: 'ClientTimeout',
            client_error: Type[Exception]=ClientConnectorError,
            **kwargs: Any) -> Tuple[asyncio.Transport, ResponseHandler]:
        try:
            with CeilTimeout(timeout.sock_connect):
                return cast(
                    Tuple[asyncio.Transport, ResponseHandler],
                    await self._loop.create_connection(*args, **kwargs))
        except cert_errors as exc:
            raise ClientConnectorCertificateError(
                req.connection_key, exc) from exc
        except ssl_errors as exc:
            raise ClientConnectorSSLError(req.connection_key, exc) from exc
        except OSError as exc:
            raise client_error(req.connection_key, exc) from exc

    async def _create_direct_connection(
            self,
            req: 'ClientRequest',
            traces: List['Trace'],
            timeout: 'ClientTimeout',
            *,
            client_error: Type[Exception]=ClientConnectorError
    ) -> Tuple[asyncio.Transport, ResponseHandler]:
        sslcontext = self._get_ssl_context(req)
        fingerprint = self._get_fingerprint(req)

        try:
            # Cancelling this lookup should not cancel the underlying lookup
            #  or else the cancel event will get broadcast to all the waiters
            #  across all connections.
            host = req.url.raw_host
            assert host is not None
            port = req.port
            assert port is not None
            hosts = await asyncio.shield(self._resolve_host(
                host,
                port,
                traces=traces), loop=self._loop)
        except OSError as exc:
            # in case of proxy it is not ClientProxyConnectionError
            # it is problem of resolving proxy ip itself
            raise ClientConnectorError(req.connection_key, exc) from exc

        last_exc = None  # type: Optional[Exception]

        for hinfo in hosts:
            host = hinfo['host']
            port = hinfo['port']

            try:
                transp, proto = await self._wrap_create_connection(
                    self._factory, host, port, timeout=timeout,
                    ssl=sslcontext, family=hinfo['family'],
                    proto=hinfo['proto'], flags=hinfo['flags'],
                    server_hostname=hinfo['hostname'] if sslcontext else None,
                    local_addr=self._local_addr,
                    req=req, client_error=client_error)
            except ClientConnectorError as exc:
                last_exc = exc
                continue

            if req.is_ssl() and fingerprint:
                try:
                    fingerprint.check(transp)
                except ServerFingerprintMismatch as exc:
                    transp.close()
                    if not self._cleanup_closed_disabled:
                        self._cleanup_closed_transports.append(transp)
                    last_exc = exc
                    continue

            return transp, proto
        else:
            assert last_exc is not None
            raise last_exc

    async def _create_proxy_connection(
            self,
            req: 'ClientRequest',
            traces: List['Trace'],
            timeout: 'ClientTimeout'
    ) -> Tuple[asyncio.Transport, ResponseHandler]:
        headers = {}  # type: Dict[str, str]
        if req.proxy_headers is not None:
            headers = req.proxy_headers  # type: ignore
        headers[hdrs.HOST] = req.headers[hdrs.HOST]

        url = req.proxy
        assert url is not None
        proxy_req = ClientRequest(
            hdrs.METH_GET, url,
            headers=headers,
            auth=req.proxy_auth,
            loop=self._loop,
            ssl=req.ssl)

        # create connection to proxy server
        transport, proto = await self._create_direct_connection(
            proxy_req, [], timeout, client_error=ClientProxyConnectionError)

        # Many HTTP proxies has buggy keepalive support.  Let's not
        # reuse connection but close it after processing every
        # response.
        proto.force_close()

        auth = proxy_req.headers.pop(hdrs.AUTHORIZATION, None)
        if auth is not None:
            if not req.is_ssl():
                req.headers[hdrs.PROXY_AUTHORIZATION] = auth
            else:
                proxy_req.headers[hdrs.PROXY_AUTHORIZATION] = auth

        if req.is_ssl():
            sslcontext = self._get_ssl_context(req)
            # For HTTPS requests over HTTP proxy
            # we must notify proxy to tunnel connection
            # so we send CONNECT command:
            #   CONNECT www.python.org:443 HTTP/1.1
            #   Host: www.python.org
            #
            # next we must do TLS handshake and so on
            # to do this we must wrap raw socket into secure one
            # asyncio handles this perfectly
            proxy_req.method = hdrs.METH_CONNECT
            proxy_req.url = req.url
            key = attr.evolve(req.connection_key,
                              proxy=None,
                              proxy_auth=None,
                              proxy_headers_hash=None)
            conn = Connection(self, key, proto, self._loop)
            proxy_resp = await proxy_req.send(conn)
            try:
                protocol = conn._protocol
                assert protocol is not None
                protocol.set_response_params()
                resp = await proxy_resp.start(conn)
            except BaseException:
                proxy_resp.close()
                conn.close()
                raise
            else:
                conn._protocol = None
                conn._transport = None
                try:
                    if resp.status != 200:
                        message = resp.reason
                        if message is None:
                            message = RESPONSES[resp.status][0]
                        raise ClientHttpProxyError(
                            proxy_resp.request_info,
                            resp.history,
                            status=resp.status,
                            message=message,
                            headers=resp.headers)
                    rawsock = transport.get_extra_info('socket', default=None)
                    if rawsock is None:
                        raise RuntimeError(
                            "Transport does not expose socket instance")
                    # Duplicate the socket, so now we can close proxy transport
                    rawsock = rawsock.dup()
                finally:
                    transport.close()

                transport, proto = await self._wrap_create_connection(
                    self._factory, timeout=timeout,
                    ssl=sslcontext, sock=rawsock,
                    server_hostname=req.host,
                    req=req)
            finally:
                proxy_resp.close()

        return transport, proto


class UnixConnector(BaseConnector):
    """Unix socket connector.

    path - Unix socket path.
    keepalive_timeout - (optional) Keep-alive timeout.
    force_close - Set to True to force close and do reconnect
        after each request (and between redirects).
    limit - The total number of simultaneous connections.
    limit_per_host - Number of simultaneous connections to one host.
    loop - Optional event loop.
    """

    def __init__(self, path: str, force_close: bool=False,
                 keepalive_timeout: Union[object, float, None]=sentinel,
                 limit: int=100, limit_per_host: int=0,
                 loop: Optional[asyncio.AbstractEventLoop]=None) -> None:
        super().__init__(force_close=force_close,
                         keepalive_timeout=keepalive_timeout,
                         limit=limit, limit_per_host=limit_per_host, loop=loop)
        self._path = path

    @property
    def path(self) -> str:
        """Path to unix socket."""
        return self._path

    async def _create_connection(self, req: 'ClientRequest',
                                 traces: List['Trace'],
                                 timeout: 'ClientTimeout') -> ResponseHandler:
        try:
            with CeilTimeout(timeout.sock_connect):
                _, proto = await self._loop.create_unix_connection(
                    self._factory, self._path)
        except OSError as exc:
            raise ClientConnectorError(req.connection_key, exc) from exc

        return cast(ResponseHandler, proto)
