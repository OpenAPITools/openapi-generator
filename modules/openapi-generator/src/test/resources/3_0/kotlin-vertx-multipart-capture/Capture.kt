import io.vertx.core.Vertx
import org.openapitools.client.apis.DefaultApi
import java.io.File
import java.net.ServerSocket
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

// Raw HTTP capture for the jvm-vertx generated client, multipart variant of
// kotlin-vertx-capture/Capture.kt (issue #17, D1): file form fields must carry
// the file CONTENT on the wire — attribute("file", file.toString()) used to
// send the local path instead. Non-file arrays must arrive as repeated parts
// (toMultiValue), not as a "[a, b]" toString blob. Vert.x may send the form
// with Transfer-Encoding: chunked, so the body is reassembled from chunks.
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

    val secret = File.createTempFile("secret-kotlin-vertx-", ".txt")
    secret.writeText("FILE-CONTENT-XYZ")
    val secret2 = File.createTempFile("secret2-kotlin-vertx-", ".txt")
    secret2.writeText("FILE2-CONTENT-UVW")

    val api = DefaultApi(vertx = vertx, basePath = "http://localhost:$port")
    try {
        api.upload(myFile = secret, files = listOf(secret2), note = "n1", tags = listOf("a", "b"))
            .toCompletionStage().toCompletableFuture().get(20, TimeUnit.SECONDS)
    } catch (e: Exception) {
        println("CALL-FAIL: $e")
    }
    latch.await(20, TimeUnit.SECONDS)
    println("---CAPTURE---")
    println(captured.toString())
    val text = captured.toString()
    val tagParts = Regex("name=\"tags\"").findAll(text).count()
    val pass = text.contains("multipart/form-data") &&
            text.contains("FILE-CONTENT-XYZ") &&
            text.contains("FILE2-CONTENT-UVW") &&
            text.contains("name=\"note\"") && text.contains("n1") &&
            tagParts == 2 && !text.contains("[a, b]") &&
            !text.contains(secret.absolutePath) && !text.contains(secret2.absolutePath)
    println(if (pass) "CAPTURE-PASS" else "CAPTURE-FAIL")
    vertx.close()
}
