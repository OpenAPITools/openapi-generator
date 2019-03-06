import asyncio
import collections
import warnings
from typing import List  # noqa
from typing import Awaitable, Callable, Generic, Optional, Tuple, TypeVar

from .base_protocol import BaseProtocol
from .helpers import BaseTimerContext, set_exception, set_result
from .log import internal_logger

try:  # pragma: no cover
    from typing import Deque  # noqa
except ImportError:
    from typing_extensions import Deque  # noqa


__all__ = (
    'EMPTY_PAYLOAD', 'EofStream', 'StreamReader', 'DataQueue',
    'FlowControlDataQueue')

DEFAULT_LIMIT = 2 ** 16

_T = TypeVar('_T')


class EofStream(Exception):
    """eof stream indication."""


class AsyncStreamIterator(Generic[_T]):

    def __init__(self, read_func: Callable[[], Awaitable[_T]]) -> None:
        self.read_func = read_func

    def __aiter__(self) -> 'AsyncStreamIterator[_T]':
        return self

    async def __anext__(self) -> _T:
        try:
            rv = await self.read_func()
        except EofStream:
            raise StopAsyncIteration  # NOQA
        if rv == b'':
            raise StopAsyncIteration  # NOQA
        return rv


class ChunkTupleAsyncStreamIterator:

    def __init__(self, stream: 'StreamReader') -> None:
        self._stream = stream

    def __aiter__(self) -> 'ChunkTupleAsyncStreamIterator':
        return self

    async def __anext__(self) -> Tuple[bytes, bool]:
        rv = await self._stream.readchunk()
        if rv == (b'', False):
            raise StopAsyncIteration  # NOQA
        return rv


class AsyncStreamReaderMixin:

    def __aiter__(self) -> AsyncStreamIterator[bytes]:
        return AsyncStreamIterator(self.readline)  # type: ignore

    def iter_chunked(self, n: int) -> AsyncStreamIterator[bytes]:
        """Returns an asynchronous iterator that yields chunks of size n.

        Python-3.5 available for Python 3.5+ only
        """
        return AsyncStreamIterator(lambda: self.read(n))  # type: ignore

    def iter_any(self) -> AsyncStreamIterator[bytes]:
        """Returns an asynchronous iterator that yields all the available
        data as soon as it is received

        Python-3.5 available for Python 3.5+ only
        """
        return AsyncStreamIterator(self.readany)  # type: ignore

    def iter_chunks(self) -> ChunkTupleAsyncStreamIterator:
        """Returns an asynchronous iterator that yields chunks of data
        as they are received by the server. The yielded objects are tuples
        of (bytes, bool) as returned by the StreamReader.readchunk method.

        Python-3.5 available for Python 3.5+ only
        """
        return ChunkTupleAsyncStreamIterator(self)  # type: ignore


class StreamReader(AsyncStreamReaderMixin):
    """An enhancement of asyncio.StreamReader.

    Supports asynchronous iteration by line, chunk or as available::

        async for line in reader:
            ...
        async for chunk in reader.iter_chunked(1024):
            ...
        async for slice in reader.iter_any():
            ...

    """

    total_bytes = 0

    def __init__(self, protocol: BaseProtocol,
                 *, limit: int=DEFAULT_LIMIT,
                 timer: Optional[BaseTimerContext]=None,
                 loop: Optional[asyncio.AbstractEventLoop]=None) -> None:
        self._protocol = protocol
        self._low_water = limit
        self._high_water = limit * 2
        if loop is None:
            loop = asyncio.get_event_loop()
        self._loop = loop
        self._size = 0
        self._cursor = 0
        self._http_chunk_splits = None  # type: Optional[List[int]]
        self._buffer = collections.deque()  # type: Deque[bytes]
        self._buffer_offset = 0
        self._eof = False
        self._waiter = None  # type: Optional[asyncio.Future[bool]]
        self._eof_waiter = None  # type: Optional[asyncio.Future[bool]]
        self._exception = None  # type: Optional[BaseException]
        self._timer = timer
        self._eof_callbacks = []  # type: List[Callable[[], None]]

    def __repr__(self) -> str:
        info = [self.__class__.__name__]
        if self._size:
            info.append('%d bytes' % self._size)
        if self._eof:
            info.append('eof')
        if self._low_water != DEFAULT_LIMIT:
            info.append('low=%d high=%d' % (self._low_water, self._high_water))
        if self._waiter:
            info.append('w=%r' % self._waiter)
        if self._exception:
            info.append('e=%r' % self._exception)
        return '<%s>' % ' '.join(info)

    def exception(self) -> Optional[BaseException]:
        return self._exception

    def set_exception(self, exc: BaseException) -> None:
        self._exception = exc
        self._eof_callbacks.clear()

        waiter = self._waiter
        if waiter is not None:
            self._waiter = None
            set_exception(waiter, exc)

        waiter = self._eof_waiter
        if waiter is not None:
            set_exception(waiter, exc)
            self._eof_waiter = None

    def on_eof(self, callback: Callable[[], None]) -> None:
        if self._eof:
            try:
                callback()
            except Exception:
                internal_logger.exception('Exception in eof callback')
        else:
            self._eof_callbacks.append(callback)

    def feed_eof(self) -> None:
        self._eof = True

        waiter = self._waiter
        if waiter is not None:
            self._waiter = None
            set_result(waiter, True)

        waiter = self._eof_waiter
        if waiter is not None:
            self._eof_waiter = None
            set_result(waiter, True)

        for cb in self._eof_callbacks:
            try:
                cb()
            except Exception:
                internal_logger.exception('Exception in eof callback')

        self._eof_callbacks.clear()

    def is_eof(self) -> bool:
        """Return True if  'feed_eof' was called."""
        return self._eof

    def at_eof(self) -> bool:
        """Return True if the buffer is empty and 'feed_eof' was called."""
        return self._eof and not self._buffer

    async def wait_eof(self) -> None:
        if self._eof:
            return

        assert self._eof_waiter is None
        self._eof_waiter = self._loop.create_future()
        try:
            await self._eof_waiter
        finally:
            self._eof_waiter = None

    def unread_data(self, data: bytes) -> None:
        """ rollback reading some data from stream, inserting it to buffer head.
        """
        warnings.warn("unread_data() is deprecated "
                      "and will be removed in future releases (#3260)",
                      DeprecationWarning,
                      stacklevel=2)
        if not data:
            return

        if self._buffer_offset:
            self._buffer[0] = self._buffer[0][self._buffer_offset:]
            self._buffer_offset = 0
        self._size += len(data)
        self._cursor -= len(data)
        self._buffer.appendleft(data)
        self._eof_counter = 0

    # TODO: size is ignored, remove the param later
    def feed_data(self, data: bytes, size: int=0) -> None:
        assert not self._eof, 'feed_data after feed_eof'

        if not data:
            return

        self._size += len(data)
        self._buffer.append(data)
        self.total_bytes += len(data)

        waiter = self._waiter
        if waiter is not None:
            self._waiter = None
            set_result(waiter, False)

        if (self._size > self._high_water and
                not self._protocol._reading_paused):
            self._protocol.pause_reading()

    def begin_http_chunk_receiving(self) -> None:
        if self._http_chunk_splits is None:
            if self.total_bytes:
                raise RuntimeError("Called begin_http_chunk_receiving when"
                                   "some data was already fed")
            self._http_chunk_splits = []

    def end_http_chunk_receiving(self) -> None:
        if self._http_chunk_splits is None:
            raise RuntimeError("Called end_chunk_receiving without calling "
                               "begin_chunk_receiving first")

        # self._http_chunk_splits contains logical byte offsets from start of
        # the body transfer. Each offset is the offset of the end of a chunk.
        # "Logical" means bytes, accessible for a user.
        # If no chunks containig logical data were received, current position
        # is difinitely zero.
        pos = self._http_chunk_splits[-1] if self._http_chunk_splits else 0

        if self.total_bytes == pos:
            # We should not add empty chunks here. So we check for that.
            # Note, when chunked + gzip is used, we can receive a chunk
            # of compressed data, but that data may not be enough for gzip FSM
            # to yield any uncompressed data. That's why current position may
            # not change after receiving a chunk.
            return

        self._http_chunk_splits.append(self.total_bytes)

        # wake up readchunk when end of http chunk received
        waiter = self._waiter
        if waiter is not None:
            self._waiter = None
            set_result(waiter, False)

    async def _wait(self, func_name: str) -> None:
        # StreamReader uses a future to link the protocol feed_data() method
        # to a read coroutine. Running two read coroutines at the same time
        # would have an unexpected behaviour. It would not possible to know
        # which coroutine would get the next data.
        if self._waiter is not None:
            raise RuntimeError('%s() called while another coroutine is '
                               'already waiting for incoming data' % func_name)

        waiter = self._waiter = self._loop.create_future()
        try:
            if self._timer:
                with self._timer:
                    await waiter
            else:
                await waiter
        finally:
            self._waiter = None

    async def readline(self) -> bytes:
        if self._exception is not None:
            raise self._exception

        line = []
        line_size = 0
        not_enough = True

        while not_enough:
            while self._buffer and not_enough:
                offset = self._buffer_offset
                ichar = self._buffer[0].find(b'\n', offset) + 1
                # Read from current offset to found b'\n' or to the end.
                data = self._read_nowait_chunk(ichar - offset if ichar else -1)
                line.append(data)
                line_size += len(data)
                if ichar:
                    not_enough = False

                if line_size > self._high_water:
                    raise ValueError('Line is too long')

            if self._eof:
                break

            if not_enough:
                await self._wait('readline')

        return b''.join(line)

    async def read(self, n: int=-1) -> bytes:
        if self._exception is not None:
            raise self._exception

        # migration problem; with DataQueue you have to catch
        # EofStream exception, so common way is to run payload.read() inside
        # infinite loop. what can cause real infinite loop with StreamReader
        # lets keep this code one major release.
        if __debug__:
            if self._eof and not self._buffer:
                self._eof_counter = getattr(self, '_eof_counter', 0) + 1
                if self._eof_counter > 5:
                    internal_logger.warning(
                        'Multiple access to StreamReader in eof state, '
                        'might be infinite loop.', stack_info=True)

        if not n:
            return b''

        if n < 0:
            # This used to just loop creating a new waiter hoping to
            # collect everything in self._buffer, but that would
            # deadlock if the subprocess sends more than self.limit
            # bytes.  So just call self.readany() until EOF.
            blocks = []
            while True:
                block = await self.readany()
                if not block:
                    break
                blocks.append(block)
            return b''.join(blocks)

        # TODO: should be `if` instead of `while`
        # because waiter maybe triggered on chunk end,
        # without feeding any data
        while not self._buffer and not self._eof:
            await self._wait('read')

        return self._read_nowait(n)

    async def readany(self) -> bytes:
        if self._exception is not None:
            raise self._exception

        # TODO: should be `if` instead of `while`
        # because waiter maybe triggered on chunk end,
        # without feeding any data
        while not self._buffer and not self._eof:
            await self._wait('readany')

        return self._read_nowait(-1)

    async def readchunk(self) -> Tuple[bytes, bool]:
        """Returns a tuple of (data, end_of_http_chunk). When chunked transfer
        encoding is used, end_of_http_chunk is a boolean indicating if the end
        of the data corresponds to the end of a HTTP chunk , otherwise it is
        always False.
        """
        if self._exception is not None:
            raise self._exception

        if not self._buffer and not self._eof:
            if (self._http_chunk_splits and
                    self._cursor == self._http_chunk_splits[0]):
                # end of http chunk without available data
                self._http_chunk_splits = self._http_chunk_splits[1:]
                return (b"", True)
            await self._wait('readchunk')

        if not self._buffer and not self._http_chunk_splits:
            # end of file
            return (b"", False)
        elif self._http_chunk_splits is not None:
            while self._http_chunk_splits:
                pos = self._http_chunk_splits[0]
                self._http_chunk_splits = self._http_chunk_splits[1:]
                if pos == self._cursor:
                    return (b"", True)
                if pos > self._cursor:
                    return (self._read_nowait(pos-self._cursor), True)
            return (self._read_nowait(-1), False)
        else:
            return (self._read_nowait_chunk(-1), False)

    async def readexactly(self, n: int) -> bytes:
        if self._exception is not None:
            raise self._exception

        blocks = []  # type: List[bytes]
        while n > 0:
            block = await self.read(n)
            if not block:
                partial = b''.join(blocks)
                raise asyncio.streams.IncompleteReadError(
                    partial, len(partial) + n)
            blocks.append(block)
            n -= len(block)

        return b''.join(blocks)

    def read_nowait(self, n: int=-1) -> bytes:
        # default was changed to be consistent with .read(-1)
        #
        # I believe the most users don't know about the method and
        # they are not affected.
        if self._exception is not None:
            raise self._exception

        if self._waiter and not self._waiter.done():
            raise RuntimeError(
                'Called while some coroutine is waiting for incoming data.')

        return self._read_nowait(n)

    def _read_nowait_chunk(self, n: int) -> bytes:
        first_buffer = self._buffer[0]
        offset = self._buffer_offset
        if n != -1 and len(first_buffer) - offset > n:
            data = first_buffer[offset:offset + n]
            self._buffer_offset += n

        elif offset:
            self._buffer.popleft()
            data = first_buffer[offset:]
            self._buffer_offset = 0

        else:
            data = self._buffer.popleft()

        self._size -= len(data)
        self._cursor += len(data)

        if self._size < self._low_water and self._protocol._reading_paused:
            self._protocol.resume_reading()
        return data

    def _read_nowait(self, n: int) -> bytes:
        chunks = []

        while self._buffer:
            chunk = self._read_nowait_chunk(n)
            chunks.append(chunk)
            if n != -1:
                n -= len(chunk)
                if n == 0:
                    break

        return b''.join(chunks) if chunks else b''


class EmptyStreamReader(AsyncStreamReaderMixin):

    def exception(self) -> Optional[BaseException]:
        return None

    def set_exception(self, exc: BaseException) -> None:
        pass

    def on_eof(self, callback: Callable[[], None]) -> None:
        try:
            callback()
        except Exception:
            internal_logger.exception('Exception in eof callback')

    def feed_eof(self) -> None:
        pass

    def is_eof(self) -> bool:
        return True

    def at_eof(self) -> bool:
        return True

    async def wait_eof(self) -> None:
        return

    def feed_data(self, data: bytes, n: int=0) -> None:
        pass

    async def readline(self) -> bytes:
        return b''

    async def read(self, n: int=-1) -> bytes:
        return b''

    async def readany(self) -> bytes:
        return b''

    async def readchunk(self) -> Tuple[bytes, bool]:
        return (b'', True)

    async def readexactly(self, n: int) -> bytes:
        raise asyncio.streams.IncompleteReadError(b'', n)

    def read_nowait(self) -> bytes:
        return b''


EMPTY_PAYLOAD = EmptyStreamReader()


class DataQueue(Generic[_T]):
    """DataQueue is a general-purpose blocking queue with one reader."""

    def __init__(self, loop: asyncio.AbstractEventLoop) -> None:
        self._loop = loop
        self._eof = False
        self._waiter = None  # type: Optional[asyncio.Future[bool]]
        self._exception = None  # type: Optional[BaseException]
        self._size = 0
        self._buffer = collections.deque()  # type: Deque[Tuple[_T, int]]

    def __len__(self) -> int:
        return len(self._buffer)

    def is_eof(self) -> bool:
        return self._eof

    def at_eof(self) -> bool:
        return self._eof and not self._buffer

    def exception(self) -> Optional[BaseException]:
        return self._exception

    def set_exception(self, exc: BaseException) -> None:
        self._eof = True
        self._exception = exc

        waiter = self._waiter
        if waiter is not None:
            set_exception(waiter, exc)
            self._waiter = None

    def feed_data(self, data: _T, size: int=0) -> None:
        self._size += size
        self._buffer.append((data, size))

        waiter = self._waiter
        if waiter is not None:
            self._waiter = None
            set_result(waiter, True)

    def feed_eof(self) -> None:
        self._eof = True

        waiter = self._waiter
        if waiter is not None:
            self._waiter = None
            set_result(waiter, False)

    async def read(self) -> _T:
        if not self._buffer and not self._eof:
            assert not self._waiter
            self._waiter = self._loop.create_future()
            try:
                await self._waiter
            except (asyncio.CancelledError, asyncio.TimeoutError):
                self._waiter = None
                raise

        if self._buffer:
            data, size = self._buffer.popleft()
            self._size -= size
            return data
        else:
            if self._exception is not None:
                raise self._exception
            else:
                raise EofStream

    def __aiter__(self) -> AsyncStreamIterator[_T]:
        return AsyncStreamIterator(self.read)


class FlowControlDataQueue(DataQueue[_T]):
    """FlowControlDataQueue resumes and pauses an underlying stream.

    It is a destination for parsed data."""

    def __init__(self, protocol: BaseProtocol, *,
                 limit: int=DEFAULT_LIMIT,
                 loop: asyncio.AbstractEventLoop) -> None:
        super().__init__(loop=loop)

        self._protocol = protocol
        self._limit = limit * 2

    def feed_data(self, data: _T, size: int=0) -> None:
        super().feed_data(data, size)

        if self._size > self._limit and not self._protocol._reading_paused:
            self._protocol.pause_reading()

    async def read(self) -> _T:
        try:
            return await super().read()
        finally:
            if self._size < self._limit and self._protocol._reading_paused:
                self._protocol.resume_reading()
