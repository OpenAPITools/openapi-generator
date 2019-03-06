from libc.stdint cimport uint16_t, uint32_t, uint64_t


cdef extern from "../vendor/http-parser/http_parser.h":
    ctypedef int (*http_data_cb) (http_parser*,
                                  const char *at,
                                  size_t length) except -1

    ctypedef int (*http_cb) (http_parser*) except -1

    struct http_parser:
        unsigned int type
        unsigned int flags
        unsigned int state
        unsigned int header_state
        unsigned int index

        uint32_t nread
        uint64_t content_length

        unsigned short http_major
        unsigned short http_minor
        unsigned int status_code
        unsigned int method
        unsigned int http_errno

        unsigned int upgrade

        void *data

    struct http_parser_settings:
        http_cb      on_message_begin
        http_data_cb on_url
        http_data_cb on_status
        http_data_cb on_header_field
        http_data_cb on_header_value
        http_cb      on_headers_complete
        http_data_cb on_body
        http_cb      on_message_complete
        http_cb      on_chunk_header
        http_cb      on_chunk_complete

    enum http_parser_type:
        HTTP_REQUEST,
        HTTP_RESPONSE,
        HTTP_BOTH

    enum http_errno:
        HPE_OK,
        HPE_CB_message_begin,
        HPE_CB_url,
        HPE_CB_header_field,
        HPE_CB_header_value,
        HPE_CB_headers_complete,
        HPE_CB_body,
        HPE_CB_message_complete,
        HPE_CB_status,
        HPE_CB_chunk_header,
        HPE_CB_chunk_complete,
        HPE_INVALID_EOF_STATE,
        HPE_HEADER_OVERFLOW,
        HPE_CLOSED_CONNECTION,
        HPE_INVALID_VERSION,
        HPE_INVALID_STATUS,
        HPE_INVALID_METHOD,
        HPE_INVALID_URL,
        HPE_INVALID_HOST,
        HPE_INVALID_PORT,
        HPE_INVALID_PATH,
        HPE_INVALID_QUERY_STRING,
        HPE_INVALID_FRAGMENT,
        HPE_LF_EXPECTED,
        HPE_INVALID_HEADER_TOKEN,
        HPE_INVALID_CONTENT_LENGTH,
        HPE_INVALID_CHUNK_SIZE,
        HPE_INVALID_CONSTANT,
        HPE_INVALID_INTERNAL_STATE,
        HPE_STRICT,
        HPE_PAUSED,
        HPE_UNKNOWN

    enum flags:
        F_CHUNKED,
        F_CONNECTION_KEEP_ALIVE,
        F_CONNECTION_CLOSE,
        F_CONNECTION_UPGRADE,
        F_TRAILING,
        F_UPGRADE,
        F_SKIPBODY,
        F_CONTENTLENGTH

    enum http_method:
        DELETE, GET, HEAD, POST, PUT, CONNECT, OPTIONS, TRACE, COPY,
        LOCK, MKCOL, MOVE, PROPFIND, PROPPATCH, SEARCH, UNLOCK, BIND,
        REBIND, UNBIND, ACL, REPORT, MKACTIVITY, CHECKOUT, MERGE,
        MSEARCH, NOTIFY, SUBSCRIBE, UNSUBSCRIBE, PATCH, PURGE, MKCALENDAR,
        LINK, UNLINK

    void http_parser_init(http_parser *parser, http_parser_type type)

    size_t http_parser_execute(http_parser *parser,
                               const http_parser_settings *settings,
                               const char *data,
                               size_t len)

    int http_should_keep_alive(const http_parser *parser)

    void http_parser_settings_init(http_parser_settings *settings)

    const char *http_errno_name(http_errno err)
    const char *http_errno_description(http_errno err)
    const char *http_method_str(http_method m)

    # URL Parser

    enum http_parser_url_fields:
        UF_SCHEMA   = 0,
        UF_HOST     = 1,
        UF_PORT     = 2,
        UF_PATH     = 3,
        UF_QUERY    = 4,
        UF_FRAGMENT = 5,
        UF_USERINFO = 6,
        UF_MAX      = 7

    struct http_parser_url_field_data:
        uint16_t off
        uint16_t len

    struct http_parser_url:
        uint16_t field_set
        uint16_t port
        http_parser_url_field_data[<int>UF_MAX] field_data

    void http_parser_url_init(http_parser_url *u)

    int http_parser_parse_url(const char *buf,
                              size_t buflen,
                              int is_connect,
                              http_parser_url *u)
