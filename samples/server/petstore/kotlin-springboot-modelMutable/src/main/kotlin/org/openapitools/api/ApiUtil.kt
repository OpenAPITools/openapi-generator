package org.openapitools.api

import org.springframework.web.context.request.NativeWebRequest

import javax.servlet.http.HttpServletResponse
import java.io.IOException

object ApiUtil {
    fun setExampleResponse(req: NativeWebRequest, contentType: String, example: String) {
        try {
            val res = req.getNativeResponse(HttpServletResponse::class.java)
            res.setCharacterEncoding("UTF-8")
            res.addHeader("Content-Type", contentType)
            res.getWriter().print(example)
        } catch (e: IOException) {
            throw RuntimeException(e)
        }
    }
}
