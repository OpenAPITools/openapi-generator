package org.openapitools.client

class EchoServerResponseParser(response: String) {
    lateinit var method: String
    lateinit var path: String
    lateinit var protocol: String
    val headers = hashMapOf<String, String>()
    val body: String

    init {
        require(response.isNotEmpty()) { "Echo server response cannot be null" }

        val lines = response.lineSequence().iterator()
        var firstLine = true
        var bodyStart = false
        val bodyBuilder = StringBuilder()
        for (line in lines) {
            if (firstLine) {
                val items = line.split(" ")
                method = items[0]
                path = items[1]
                protocol = items[2]
                firstLine = false
                continue
            }
            if (bodyStart) {
                bodyBuilder.append(line)
                bodyBuilder.append("\n")
            }
            if (line.isEmpty()) {
                bodyStart = true
                continue
            }

            val keyValue = line.split(": ")
            if (keyValue.size == 2) {
                headers[keyValue[0]] = keyValue[1]
            }
        }
        body = bodyBuilder.toString().trimEnd()
    }
}


