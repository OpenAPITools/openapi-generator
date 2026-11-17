package org.openapitools.client;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import okhttp3.MediaType;
import okhttp3.RequestBody;
import okio.Buffer;
import okio.BufferedSink;
import okio.Okio;
import okio.Sink;
import org.junit.jupiter.api.Test;

public class ProgressRequestBodyTest {

    private static class RecordingCallback implements ApiCallback<Object> {
        final List<Boolean> doneFlags = new ArrayList<>();
        long lastBytesWritten = -1;

        @Override
        public void onFailure(ApiException e, int statusCode, Map<String, List<String>> responseHeaders) {
        }

        @Override
        public void onSuccess(Object result, int statusCode, Map<String, List<String>> responseHeaders) {
        }

        @Override
        public void onUploadProgress(long bytesWritten, long contentLength, boolean done) {
            doneFlags.add(done);
            lastBytesWritten = bytesWritten;
        }

        @Override
        public void onDownloadProgress(long bytesRead, long contentLength, boolean done) {
        }
    }

    @Test
    public void nonDuplexBodyReportsCompletionWhenWriteToReturns() throws IOException {
        RecordingCallback callback = new RecordingCallback();
        ProgressRequestBody body = new ProgressRequestBody(
                RequestBody.create("hello", MediaType.get("text/plain")), callback);

        Buffer out = new Buffer();
        BufferedSink sink = Okio.buffer((Sink) out);
        body.writeTo(sink);
        sink.flush();

        assertEquals("hello", out.readUtf8());
        assertFalse(callback.doneFlags.isEmpty());
        assertTrue(callback.doneFlags.get(callback.doneFlags.size() - 1), "last event must be the terminal one");
        assertEquals(5, callback.lastBytesWritten);
    }

    @Test
    public void duplexBodyReportsCompletionOnlyWhenTheDelegateClosesTheSink() throws IOException {
        RecordingCallback callback = new RecordingCallback();
        BufferedSink[] captured = new BufferedSink[1];
        RequestBody duplex = new RequestBody() {
            @Override
            public MediaType contentType() {
                return MediaType.get("application/octet-stream");
            }

            @Override
            public boolean isDuplex() {
                return true;
            }

            @Override
            public void writeTo(BufferedSink sink) throws IOException {
                // a duplex body hands the sink to another writer and returns before the upload is over
                sink.writeUtf8("part1");
                sink.flush();
                captured[0] = sink;
            }
        };
        ProgressRequestBody body = new ProgressRequestBody(duplex, callback);
        assertTrue(body.isDuplex());

        Buffer out = new Buffer();
        body.writeTo(Okio.buffer((Sink) out));
        assertFalse(callback.doneFlags.contains(true), "completion must not be reported while the delegate may still write");

        captured[0].writeUtf8("part2");
        captured[0].flush();
        assertFalse(callback.doneFlags.contains(true));

        captured[0].close();
        assertEquals("part1part2", out.readUtf8());
        assertTrue(callback.doneFlags.get(callback.doneFlags.size() - 1), "closing the sink is the completion signal");
        assertEquals(10, callback.lastBytesWritten);
    }
}
