package org.openapitools.codegen.templating.mustache;

import java.util.Map;

public class HttpStatusCodesMap {
    public static final Map<Integer, String> STATUS_CODES = Map.<Integer, String>ofEntries(
            Map.entry(100, "Continue"), // RFC 9110, 15.2.1
            Map.entry(101, "Switching Protocols"), // RFC 9110, 15.2.2
            Map.entry(102, "Processing"), // RFC 2518, 10.1
            Map.entry(103, "Early Hints"), // RFC 8297

            Map.entry(200, "OK"), // RFC 9110, 15.3.1
            Map.entry(201, "Created"), // RFC 9110, 15.3.2
            Map.entry(202, "Accepted"), // RFC 9110, 15.3.3
            Map.entry(203, "Non-Authoritative Information"), // RFC 9110, 15.3.4
            Map.entry(204, "No Content"), // RFC 9110, 15.3.5
            Map.entry(205, "Reset Content"), // RFC 9110, 15.3.6
            Map.entry(206, "Partial Content"), // RFC 9110, 15.3.7
            Map.entry(207, "Multi-Status"), // RFC 4918, 11.1
            Map.entry(208, "Already Reported"), // RFC 5842, 7.1
            Map.entry(226, "IM Used"), // RFC 3229, 10.4.1

            Map.entry(300, "Multiple Choices"), // RFC 9110, 15.4.1
            Map.entry(301, "Moved Permanently"), // RFC 9110, 15.4.2
            Map.entry(302, "Found"), // RFC 9110, 15.4.3
            Map.entry(303, "See Other"), // RFC 9110, 15.4.4
            Map.entry(304, "Not Modified"), // RFC 9110, 15.4.5
            Map.entry(305, "Use Proxy"), // RFC 9110, 15.4.6
            Map.entry(307, "Temporary Redirect"), // RFC 9110, 15.4.8
            Map.entry(308, "Permanent Redirect"), // RFC 9110, 15.4.9

            Map.entry(400, "Bad Request"), // RFC 9110, 15.5.1
            Map.entry(401, "Unauthorized"), // RFC 9110, 15.5.2
            Map.entry(402, "Payment Required"), // RFC 9110, 15.5.3
            Map.entry(403, "Forbidden"), // RFC 9110, 15.5.4
            Map.entry(404, "Not Found"), // RFC 9110, 15.5.5
            Map.entry(405, "Method Not Allowed"), // RFC 9110, 15.5.6
            Map.entry(406, "Not Acceptable"), // RFC 9110, 15.5.7
            Map.entry(407, "Proxy Authentication Required"), // RFC 9110, 15.5.8
            Map.entry(408, "Request Timeout"), // RFC 9110, 15.5.9
            Map.entry(409, "Conflict"), // RFC 9110, 15.5.10
            Map.entry(410, "Gone"), // RFC 9110, 15.5.11
            Map.entry(411, "Length Required"), // RFC 9110, 15.5.12
            Map.entry(412, "Precondition Failed"), // RFC 9110, 15.5.13
            Map.entry(413, "Payload Too Large"), // RFC 9110, 15.5.14
            Map.entry(414, "URI Too Long"), // RFC 9110, 15.5.15
            Map.entry(415, "Unsupported Media Type"), // RFC 9110, 15.5.16
            Map.entry(416, "Range Not Satisfiable"), // RFC 9110, 15.5.17
            Map.entry(417, "Expectation Failed"), // RFC 9110, 15.5.18
            Map.entry(418, "I'm a Teapot"), // RFC 9110, 15.5.19
            Map.entry(421, "Misdirected Request"), // RFC 9110, 15.5.20
            Map.entry(422, "Unprocessable Entity"), // RFC 4918, 11.3
            Map.entry(423, "Locked"), // RFC 4918, 11.3
            Map.entry(424, "Failed Dependency"), // RFC 4918, 11.4
            Map.entry(425, "Too Early"), // RFC 8470, 5.2
            Map.entry(426, "Upgrade Required"), // RFC 9110, 15.5.22
            Map.entry(428, "Precondition Required"), // RFC 6585, 3
            Map.entry(429, "Too Many Requests"), // RFC 6585, 4
            Map.entry(431, "Request Header Fields Too Large"), // RFC 6585, 5
            Map.entry(451, "Unavailable For Legal Reasons"), // RFC 7725, 3

            Map.entry(500, "Internal Server Error"), // RFC 9110, 15.6.1
            Map.entry(501, "Not Implemented"), // RFC 9110, 15.6.2
            Map.entry(502, "Bad Gateway"), // RFC 9110, 15.6.3
            Map.entry(503, "Service Unavailable"), // RFC 9110, 15.6.4
            Map.entry(504, "Gateway Timeout"), // RFC 9110, 15.6.5
            Map.entry(505, "HTTP Version Not Supported"), // RFC 9110, 15.6.6
            Map.entry(506, "Variant Also Negotiates"), // RFC 2295, 8.1
            Map.entry(507, "Insufficient Storage"), // RFC 4918, 11.5
            Map.entry(508, "Loop Detected"), // RFC 5842, 7.2
            Map.entry(510, "Not Extended"), // RFC 2774, 7
            Map.entry(511, "Network Authentication Required") // RFC 6585, 6
    );
}
