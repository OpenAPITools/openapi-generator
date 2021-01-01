require "./response"

module Crest
  # Hash of HTTP status code => message.
  STATUSES = {100 => "Continue",
              101 => "Switching Protocols",
              102 => "Processing", # WebDAV

              200 => "OK",
              201 => "Created",
              202 => "Accepted",
              203 => "Non-Authoritative Information", # http/1.1
              204 => "No Content",
              205 => "Reset Content",
              206 => "Partial Content",
              207 => "Multi-Status",     # WebDAV
              208 => "Already Reported", # RFC5842
              226 => "IM Used",          # RFC3229

              300 => "Multiple Choices",
              301 => "Moved Permanently",
              302 => "Found",
              303 => "See Other", # http/1.1
              304 => "Not Modified",
              305 => "Use Proxy",          # http/1.1
              306 => "Switch Proxy",       # no longer used
              307 => "Temporary Redirect", # http/1.1
              308 => "Permanent Redirect", # RFC7538

              400 => "Bad Request",
              401 => "Unauthorized",
              402 => "Payment Required",
              403 => "Forbidden",
              404 => "Not Found",
              405 => "Method Not Allowed",
              406 => "Not Acceptable",
              407 => "Proxy Authentication Required",
              408 => "Request Timeout",
              409 => "Conflict",
              410 => "Gone",
              411 => "Length Required",
              412 => "Precondition Failed",
              413 => "Payload Too Large", # RFC7231 (renamed, see below)
              414 => "URI Too Long",      # RFC7231 (renamed, see below)
              415 => "Unsupported Media Type",
              416 => "Range Not Satisfiable", # RFC7233 (renamed, see below)
              417 => "Expectation Failed",
              418 => "I\"m A Teapot", # RFC2324
              421 => "Too Many Connections From This IP",
              422 => "Unprocessable Entity", # WebDAV
              423 => "Locked",               # WebDAV
              424 => "Failed Dependency",    # WebDAV
              425 => "Unordered Collection", # WebDAV
              426 => "Upgrade Required",
              428 => "Precondition Required",                # RFC6585
              429 => "Too Many Requests",                    # RFC6585
              431 => "Request Header Fields Too Large",      # RFC6585
              449 => "Retry With",                           # Microsoft
              450 => "Blocked By Windows Parental Controls", # Microsoft

              500 => "Internal Server Error",
              501 => "Not Implemented",
              502 => "Bad Gateway",
              503 => "Service Unavailable",
              504 => "Gateway Timeout",
              505 => "HTTP Version Not Supported",
              506 => "Variant Also Negotiates",
              507 => "Insufficient Storage",     # WebDAV
              508 => "Loop Detected",            # RFC5842
              509 => "Bandwidth Limit Exceeded", # Apache
              510 => "Not Extended",
              511 => "Network Authentication Required", # RFC6585
  }

  # This is the base `Crest` exception class. Rescue it if you want to
  # catch any exception that your request might raise
  # You can see anything about the response via `e.response`.
  # For example, the entire result body (which is
  # probably an HTML error page) is `e.response.body`.
  #
  # Hash of HTTP status `code => message`.
  #
  # * **1xx**: Informational - Request received, continuing process
  # * **2xx**: Success - The action was successfully received, understood, and
  #    accepted
  # * **3xx**: Redirection - Further action must be taken in order to complete the
  #    request
  # * **4xx**: Client Error - The request contains bad syntax or cannot be fulfilled
  # * **5xx**: Server Error - The server failed to fulfill an apparently valid
  #    request
  #
  # See [HTTP Status Code Registry](http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml)
  # for more Information.
  class RequestFailed < Exception
    getter response

    @response : Crest::Response

    def self.subclass_by_status_code(status_code)
      EXCEPTIONS_MAP.fetch(status_code, self)
    end

    def initialize(response)
      @response = response
    end

    def http_code
      @response.status_code.to_i
    end

    def message
      "HTTP status code #{http_code}: #{STATUSES[http_code]}"
    end
  end

  EXCEPTIONS_MAP = {} of Int32 => Crest::RequestFailed.class

  {% for code, status in STATUSES %}
    # :nodoc:
    class {{status.gsub(/\W/, "").id}} < RequestFailed
    end

    EXCEPTIONS_MAP[{{code.id}}] = Crest::{{status.gsub(/\W/, "").id}}
  {% end %}
end
