#ifndef BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_
#define BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_

#include <functional>
#include <map>
#include <string>
#include <utility>

#include <boost/beast/http/status.hpp>


namespace org {
namespace openapitools {
namespace client {
namespace api {
class HttpClient {

public:
    HttpClient() = default;
    virtual ~HttpClient() = default;

    virtual std::pair<boost::beast::http::status, std::string>
    execute(const std::string &verb,
            const std::string &target,
            const std::string &body,
            const std::map<std::string, std::string> &headers) = 0;

    /// Execute an HTTP request and yield complete SSE events incrementally
    /// via callback. Used by text/event-stream endpoints.
    /// Each onEvent invocation receives one event's data payload (multi-line
    /// data fields joined by LF), framed per the WHATWG SSE specification.
    /// Incomplete events at EOF are discarded. Returns the HTTP status code.
    /// Throws std::invalid_argument if onEvent is empty.
    virtual boost::beast::http::status
    executeStream(const std::string &verb,
                  const std::string &target,
                  const std::string &body,
                  const std::map<std::string, std::string> &headers,
                  std::function<void(const std::string &)> onEvent) = 0;
};


}
}
}
}

#endif /* BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_ */
