import asyncio
import mimetypes
import os
import pathlib
from functools import partial
from typing import (  # noqa
    IO,
    TYPE_CHECKING,
    Any,
    Awaitable,
    Callable,
    List,
    Optional,
    Union,
    cast,
)

from . import hdrs
from .abc import AbstractStreamWriter
from .base_protocol import BaseProtocol
from .helpers import set_exception, set_result
from .http_writer import StreamWriter
from .log import server_logger
from .typedefs import LooseHeaders
from .web_exceptions import (
    HTTPNotModified,
    HTTPOk,
    HTTPPartialContent,
    HTTPPreconditionFailed,
    HTTPRequestRangeNotSatisfiable,
)
from .web_response import StreamResponse

__all__ = ('FileResponse',)

if TYPE_CHECKING:  # pragma: no cover
    from .web_request import BaseRequest  # noqa


_T_OnChunkSent = Optional[Callable[[bytes], Awaitable[None]]]


NOSENDFILE = bool(os.environ.get("AIOHTTP_NOSENDFILE"))


class SendfileStreamWriter(StreamWriter):

    def __init__(self,
                 protocol: BaseProtocol,
                 loop: asyncio.AbstractEventLoop,
                 fobj: IO[Any],
                 count: int,
                 on_chunk_sent: _T_OnChunkSent=None) -> None:
        super().__init__(protocol, loop, on_chunk_sent)
        self._sendfile_buffer = []  # type: List[bytes]
        self._fobj = fobj
        self._count = count
        self._offset = fobj.tell()
        self._in_fd = fobj.fileno()

    def _write(self, chunk: bytes) -> None:
        # we overwrite StreamWriter._write, so nothing can be appended to
        # _buffer, and nothing is written to the transport directly by the
        # parent class
        self.output_size += len(chunk)
        self._sendfile_buffer.append(chunk)

    def _sendfile_cb(self, fut: 'asyncio.Future[None]', out_fd: int) -> None:
        if fut.cancelled():
            return
        try:
            if self._do_sendfile(out_fd):
                set_result(fut, None)
        except Exception as exc:
            set_exception(fut, exc)

    def _do_sendfile(self, out_fd: int) -> bool:
        try:
            n = os.sendfile(out_fd,
                            self._in_fd,
                            self._offset,
                            self._count)
            if n == 0:  # in_fd EOF reached
                n = self._count
        except (BlockingIOError, InterruptedError):
            n = 0
        self.output_size += n
        self._offset += n
        self._count -= n
        assert self._count >= 0
        return self._count == 0

    def _done_fut(self, out_fd: int, fut: 'asyncio.Future[None]') -> None:
        self.loop.remove_writer(out_fd)

    async def sendfile(self) -> None:
        assert self.transport is not None
        out_socket = self.transport.get_extra_info('socket').dup()
        out_socket.setblocking(False)
        out_fd = out_socket.fileno()

        loop = self.loop
        data = b''.join(self._sendfile_buffer)
        try:
            await loop.sock_sendall(out_socket, data)
            if not self._do_sendfile(out_fd):
                fut = loop.create_future()
                fut.add_done_callback(partial(self._done_fut, out_fd))
                loop.add_writer(out_fd, self._sendfile_cb, fut, out_fd)
                await fut
        except asyncio.CancelledError:
            raise
        except Exception:
            server_logger.debug('Socket error')
            self.transport.close()
        finally:
            out_socket.close()

        await super().write_eof()

    async def write_eof(self, chunk: bytes=b'') -> None:
        pass


class FileResponse(StreamResponse):
    """A response object can be used to send files."""

    def __init__(self, path: Union[str, pathlib.Path],
                 chunk_size: int=256*1024,
                 status: int=200,
                 reason: Optional[str]=None,
                 headers: Optional[LooseHeaders]=None) -> None:
        super().__init__(status=status, reason=reason, headers=headers)

        if isinstance(path, str):
            path = pathlib.Path(path)

        self._path = path
        self._chunk_size = chunk_size

    async def _sendfile_system(self, request: 'BaseRequest',
                               fobj: IO[Any],
                               count: int) -> AbstractStreamWriter:
        # Write count bytes of fobj to resp using
        # the os.sendfile system call.
        #
        # For details check
        # https://github.com/KeepSafe/aiohttp/issues/1177
        # See https://github.com/KeepSafe/aiohttp/issues/958 for details
        #
        # request should be an aiohttp.web.Request instance.
        # fobj should be an open file object.
        # count should be an integer > 0.

        transport = request.transport
        assert transport is not None
        if (transport.get_extra_info("sslcontext") or
                transport.get_extra_info("socket") is None or
                self.compression):
            writer = await self._sendfile_fallback(request, fobj, count)
        else:
            writer = SendfileStreamWriter(
                request.protocol,
                request._loop,
                fobj,
                count
            )
            request._payload_writer = writer

            await super().prepare(request)
            await writer.sendfile()

        return writer

    async def _sendfile_fallback(self, request: 'BaseRequest',
                                 fobj: IO[Any],
                                 count: int) -> AbstractStreamWriter:
        # Mimic the _sendfile_system() method, but without using the
        # os.sendfile() system call. This should be used on systems
        # that don't support the os.sendfile().

        # To keep memory usage low,fobj is transferred in chunks
        # controlled by the constructor's chunk_size argument.

        writer = await super().prepare(request)
        assert writer is not None

        chunk_size = self._chunk_size
        loop = asyncio.get_event_loop()

        chunk = await loop.run_in_executor(None, fobj.read, chunk_size)
        while chunk:
            await writer.write(chunk)
            count = count - chunk_size
            if count <= 0:
                break
            chunk = await loop.run_in_executor(
                None, fobj.read, min(chunk_size, count)
            )

        await writer.drain()
        return writer

    if hasattr(os, "sendfile") and not NOSENDFILE:  # pragma: no cover
        _sendfile = _sendfile_system
    else:  # pragma: no cover
        _sendfile = _sendfile_fallback

    async def prepare(
            self,
            request: 'BaseRequest'
    ) -> Optional[AbstractStreamWriter]:
        filepath = self._path

        gzip = False
        if 'gzip' in request.headers.get(hdrs.ACCEPT_ENCODING, ''):
            gzip_path = filepath.with_name(filepath.name + '.gz')

            if gzip_path.is_file():
                filepath = gzip_path
                gzip = True

        loop = asyncio.get_event_loop()
        st = await loop.run_in_executor(None, filepath.stat)

        modsince = request.if_modified_since
        if modsince is not None and st.st_mtime <= modsince.timestamp():
            self.set_status(HTTPNotModified.status_code)
            self._length_check = False
            # Delete any Content-Length headers provided by user. HTTP 304
            # should always have empty response body
            return await super().prepare(request)

        unmodsince = request.if_unmodified_since
        if unmodsince is not None and st.st_mtime > unmodsince.timestamp():
            self.set_status(HTTPPreconditionFailed.status_code)
            return await super().prepare(request)

        if hdrs.CONTENT_TYPE not in self.headers:
            ct, encoding = mimetypes.guess_type(str(filepath))
            if not ct:
                ct = 'application/octet-stream'
            should_set_ct = True
        else:
            encoding = 'gzip' if gzip else None
            should_set_ct = False

        status = HTTPOk.status_code
        file_size = st.st_size
        count = file_size

        start = None

        ifrange = request.if_range
        if ifrange is None or st.st_mtime <= ifrange.timestamp():
            # If-Range header check:
            # condition = cached date >= last modification date
            # return 206 if True else 200.
            # if False:
            #   Range header would not be processed, return 200
            # if True but Range header missing
            #   return 200
            try:
                rng = request.http_range
                start = rng.start
                end = rng.stop
            except ValueError:
                # https://tools.ietf.org/html/rfc7233:
                # A server generating a 416 (Range Not Satisfiable) response to
                # a byte-range request SHOULD send a Content-Range header field
                # with an unsatisfied-range value.
                # The complete-length in a 416 response indicates the current
                # length of the selected representation.
                #
                # Will do the same below. Many servers ignore this and do not
                # send a Content-Range header with HTTP 416
                self.headers[hdrs.CONTENT_RANGE] = 'bytes */{0}'.format(
                    file_size)
                self.set_status(HTTPRequestRangeNotSatisfiable.status_code)
                return await super().prepare(request)

            # If a range request has been made, convert start, end slice
            # notation into file pointer offset and count
            if start is not None or end is not None:
                if start < 0 and end is None:  # return tail of file
                    start += file_size
                    if start < 0:
                        # if Range:bytes=-1000 in request header but file size
                        # is only 200, there would be trouble without this
                        start = 0
                    count = file_size - start
                else:
                    # rfc7233:If the last-byte-pos value is
                    # absent, or if the value is greater than or equal to
                    # the current length of the representation data,
                    # the byte range is interpreted as the remainder
                    # of the representation (i.e., the server replaces the
                    # value of last-byte-pos with a value that is one less than
                    # the current length of the selected representation).
                    count = min(end if end is not None else file_size,
                                file_size) - start

                if start >= file_size:
                    # HTTP 416 should be returned in this case.
                    #
                    # According to https://tools.ietf.org/html/rfc7233:
                    # If a valid byte-range-set includes at least one
                    # byte-range-spec with a first-byte-pos that is less than
                    # the current length of the representation, or at least one
                    # suffix-byte-range-spec with a non-zero suffix-length,
                    # then the byte-range-set is satisfiable. Otherwise, the
                    # byte-range-set is unsatisfiable.
                    self.headers[hdrs.CONTENT_RANGE] = 'bytes */{0}'.format(
                        file_size)
                    self.set_status(HTTPRequestRangeNotSatisfiable.status_code)
                    return await super().prepare(request)

                status = HTTPPartialContent.status_code
                # Even though you are sending the whole file, you should still
                # return a HTTP 206 for a Range request.

        self.set_status(status)
        if should_set_ct:
            self.content_type = ct  # type: ignore
        if encoding:
            self.headers[hdrs.CONTENT_ENCODING] = encoding
        if gzip:
            self.headers[hdrs.VARY] = hdrs.ACCEPT_ENCODING
        self.last_modified = st.st_mtime  # type: ignore
        self.content_length = count

        self.headers[hdrs.ACCEPT_RANGES] = 'bytes'

        real_start = cast(int, start)

        if status == HTTPPartialContent.status_code:
            self.headers[hdrs.CONTENT_RANGE] = 'bytes {0}-{1}/{2}'.format(
                real_start, real_start + count - 1, file_size)

        with (await loop.run_in_executor(None, filepath.open, 'rb')) as fobj:
            if start:  # be aware that start could be None or int=0 here.
                await loop.run_in_executor(None, fobj.seek, start)

            return await self._sendfile(request, fobj, count)
