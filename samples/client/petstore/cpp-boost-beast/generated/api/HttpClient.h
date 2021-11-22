#ifndef BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_
#define BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_

#include <stdexcept>
#include <string>
#include <map>

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
            const std::map<std::string, std::string> headers) = 0;
};


}
}
}
}

#endif /* BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_ */
