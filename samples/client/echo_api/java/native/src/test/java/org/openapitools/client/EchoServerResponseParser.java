package org.openapitools.client;

public class EchoServerResponseParser {
    public String method; // e.g. GET
    public String path; // e.g. /query/style_form/explode_true/object?id=12345
    public String protocol; // e.g. HTTP/1.1
    public java.util.HashMap<String, String> headers = new java.util.HashMap<>();

    public EchoServerResponseParser(String response) {
        if (response == null) {
            throw new RuntimeException("Echo server response cannot be null");
        }

        String[] lines = response.split("\n");
        boolean firstLine = true;

        for (String line : lines) {
            if (firstLine) {
                String[] items = line.split(" ");
                this.method = items[0];
                this.path = items[1];
                this.protocol = items[2];
                firstLine = false;
                continue;
            }

            // store the header key-value pair in headers
            String[] keyValue = line.split(": ");
            if (keyValue.length == 2) { // skip blank line, non key-value pair
                this.headers.put(keyValue[0], keyValue[1]);
            }
        }

    }
}
