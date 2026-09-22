import org.openapitools.client.apis.DefaultApi
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.ServerSocket
import java.net.Socket
import java.util.concurrent.CopyOnWriteArrayList
import kotlin.concurrent.thread

/**
 * Raw-TCP capture: verifies the generated jvm-okhttp client puts OpenAPI 3.2
 * query/additionalOperations method tokens and in:querystring values on the
 * wire verbatim (same approach as the Rust/Ruby/PHP/C# capture harnesses).
 */
val captured = CopyOnWriteArrayList<String>()

fun handle(s: Socket) {
    s.use {
        val reader = BufferedReader(InputStreamReader(it.getInputStream(), Charsets.ISO_8859_1))
        val requestLine = reader.readLine() ?: return
        var contentLength = 0
        while (true) {
            val h = reader.readLine() ?: break
            if (h.isEmpty()) break
            if (h.lowercase().startsWith("content-length:")) contentLength = h.substring(15).trim().toInt()
        }
        repeat(contentLength) { reader.read() }
        captured.add("$requestLine [Content-Length=$contentLength]")
        val out = it.getOutputStream()
        out.write("HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n".toByteArray(Charsets.ISO_8859_1))
        out.flush()
    }
}

fun main() {
    val server = ServerSocket(0)
    val port = server.localPort
    thread(isDaemon = true) {
        try {
            while (true) handle(server.accept())
        } catch (_: Exception) {
        }
    }

    val api = DefaultApi(basePath = "http://127.0.0.1:$port")

    fun run(label: String, block: () -> Unit) {
        try {
            block()
            println("$label -> wire=\"${captured.last()}\"")
        } catch (e: Throwable) {
            println("$label -> EXCEPTION ${e::class.simpleName}: ${e.message}")
        }
    }

    run("GET listPets (regression)") { api.listPetsWithHttpInfo() }
    run("QUERY queryPets(qs)") { api.queryPetsWithHttpInfo("a=1&b=%20x&c=x+y&d=%2F") }
    run("PURGE purgePets") { api.purgePetsWithHttpInfo() }
    run("customMethod customPets") { api.customPetsWithHttpInfo() }
    run("CHECK&FETCH checkFetchPets") { api.checkFetchPetsWithHttpInfo() }
    run("X#Y hashPets") { api.hashPetsWithHttpInfo() }
    run("A|B pipePets") { api.pipePetsWithHttpInfo() }
    run("A\$B dollarPets") { api.dollarPetsWithHttpInfo() }

    val expected = listOf(
        "GET /pets HTTP/1.1 [Content-Length=0]",
        "QUERY /pets?a=1&b=%20x&c=x+y&d=%2F HTTP/1.1 [Content-Length=0]",
        "PURGE /pets HTTP/1.1 [Content-Length=0]",
        "customMethod /pets HTTP/1.1 [Content-Length=0]",
        "CHECK&FETCH /pets HTTP/1.1 [Content-Length=0]",
        "X#Y /pets HTTP/1.1 [Content-Length=0]",
        "A|B /pets HTTP/1.1 [Content-Length=0]",
        "A\$B /pets HTTP/1.1 [Content-Length=0]"
    )
    if (captured.toList() == expected) {
        println("CAPTURE-PASS")
    } else {
        println("CAPTURE-FAIL\n  expected=$expected\n  actual=${captured.toList()}")
    }
    server.close()
}
