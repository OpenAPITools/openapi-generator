import io.vertx.core.Vertx
import org.openapitools.client.apis.DefaultApi
import java.net.ServerSocket
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

// Raw HTTP capture for the jvm-vertx generated client: form fields must
// actually reach the wire (issue #17: localVariableForm was filled but never
// sent before the sendForm/sendMultipartForm fix). Vert.x sends the form with
// Transfer-Encoding: chunked, so the body is reassembled from chunks.
fun main() {
    val vertx = Vertx.vertx()
    val server = ServerSocket(0)
    val port = server.localPort
    val captured = StringBuilder()
    val latch = CountDownLatch(1)
    Thread {
        try {
            server.accept().use { socket ->
                val reader = socket.getInputStream().bufferedReader()
                var contentLength = 0
                var chunked = false
                var line = reader.readLine()
                while (line != null && line.isNotEmpty()) {
                    captured.append(line).append("\n")
                    if (line.startsWith("Content-Length:", ignoreCase = true)) {
                        contentLength = line.substringAfter(':').trim().toInt()
                    }
                    if (line.contains("chunked", ignoreCase = true)) {
                        chunked = true
                    }
                    line = reader.readLine()
                }
                if (chunked) {
                    var sizeLine = reader.readLine()
                    while (sizeLine != null) {
                        val size = sizeLine.trim().takeWhile { it.isDigit() || it in 'a'..'f' || it in 'A'..'F' }
                            .toIntOrNull(16) ?: 0
                        if (size == 0) break
                        val chunk = CharArray(size)
                        var off = 0
                        while (off < size) {
                            val n = reader.read(chunk, off, size - off)
                            if (n < 0) break
                            off += n
                        }
                        captured.append(String(chunk, 0, off))
                        reader.readLine() // trailing CRLF
                        sizeLine = reader.readLine()
                    }
                } else {
                    val body = CharArray(contentLength)
                    var off = 0
                    while (off < contentLength) {
                        val n = reader.read(body, off, contentLength - off)
                        if (n < 0) break
                        off += n
                    }
                    captured.append(String(body, 0, off))
                }
                socket.getOutputStream().write(
                    "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n".toByteArray())
                socket.getOutputStream().flush()
            }
        } finally {
            latch.countDown()
        }
    }.start()

    val api = DefaultApi(vertx = vertx, basePath = "http://localhost:$port")
    try {
        api.createItem(`it` = "v1", name = "n2")
            .toCompletionStage().toCompletableFuture().get(20, TimeUnit.SECONDS)
    } catch (e: Exception) {
        println("CALL-FAIL: $e")
    }
    latch.await(20, TimeUnit.SECONDS)
    println("---CAPTURE---")
    println(captured.toString())
    val text = captured.toString()
    val pass = text.contains("it=v1") && text.contains("name=n2") &&
            (text.contains("application/x-www-form-urlencoded") || text.contains("multipart/form-data"))
    println(if (pass) "CAPTURE-PASS" else "CAPTURE-FAIL")
    vertx.close()
}
