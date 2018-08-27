package org.openapitools.api;

import org.springframework.web.context.request.NativeWebRequest;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class ApiUtil {
    public static void setExampleResponse(NativeWebRequest req, String contentType, String example) {
        try {
            req.getNativeResponse(HttpServletResponse.class).addHeader("Content-Type", contentType);
            req.getNativeResponse(HttpServletResponse.class).getOutputStream().print(example);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}