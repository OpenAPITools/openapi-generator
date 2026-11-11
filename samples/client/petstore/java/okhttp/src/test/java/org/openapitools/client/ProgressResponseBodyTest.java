package org.openapitools.client;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import okhttp3.MediaType;
import okhttp3.ResponseBody;
import okio.Buffer;
import okio.BufferedSource;
import org.junit.jupiter.api.Test;

public class ProgressResponseBodyTest {

    private static class RecordingCallback implements ApiCallback<Object> {
        final List<Boolean> doneFlags = new ArrayList<>();
        long lastBytesRead = -1;

        @Override
        public void onFailure(ApiException e, int statusCode, Map<String, List<String>> responseHeaders) {
        }

        @Override
        public void onSuccess(Object result, int statusCode, Map<String, List<String>> responseHeaders) {
        }

        @Override
        public void onUploadProgress(long bytesWritten, long contentLength, boolean done) {
        }

        @Override
        public void onDownloadProgress(long bytesRead, long contentLength, boolean done) {
            doneFlags.add(done);
            lastBytesRead = bytesRead;
        }
    }

    @Test
    public void completionIsReportedOnceEvenWhenTheExhaustedSourceIsReadAgain() throws IOException {
        RecordingCallback callback = new RecordingCallback();
        ProgressResponseBody body = new ProgressResponseBody(
                ResponseBody.create("hello", MediaType.get("text/plain")), callback);

        BufferedSource source = body.source();
        assertEquals("hello", source.readUtf8());
        // an exhausted source answers -1 again for every further read
        assertEquals(-1, source.read(new Buffer(), 1));
        assertEquals(-1, source.read(new Buffer(), 1));
        assertTrue(source.exhausted());

        long completions = callback.doneFlags.stream().filter(done -> done).count();
        assertEquals(1, completions, "done must be reported exactly once: " + callback.doneFlags);
        assertTrue(callback.doneFlags.get(callback.doneFlags.size() - 1), "the terminal event comes last");
        assertEquals(5, callback.lastBytesRead);
    }
}
