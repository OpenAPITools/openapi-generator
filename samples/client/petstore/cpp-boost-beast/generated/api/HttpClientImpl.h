#ifndef BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_IMPL_
#define BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_IMPL_

#include <boost/asio.hpp>
#include <boost/asio/connect.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>

#include <string>

#include "api/HttpClient.h"


namespace org {
namespace openapitools {
namespace client {
namespace api {


class HttpClientImpl : public HttpClient {

public:
    HttpClientImpl(const std::string &host,
                   const std::string &port,
                   const int httpVersion = 11); // HTTP 1.1
    virtual ~HttpClientImpl() = default;

    std::pair<boost::beast::http::status, std::string>
    execute(const std::string &verb, const std::string &target,
            const std::string &body,
            const std::map<std::string, std::string> headers) override;

protected:
    virtual boost::asio::ip::tcp::socket connectSocket();

    virtual boost::beast::http::request<boost::beast::http::string_body>
    prepareRequest(const std::string &verb, const std::string &target,
                   const std::string &body,
                   const std::map<std::string, std::string> headers);

    virtual void sendRequest(
        boost::asio::ip::tcp::socket &socket,
        boost::beast::http::request<boost::beast::http::string_body> &request);

    virtual std::pair<boost::beast::http::status, std::string>
    receiveResponse(boost::asio::ip::tcp::socket &socket);

    virtual void closeSocket(boost::asio::ip::tcp::socket &socket);

private:
    const std::string m_host;
    const std::string m_port;
    const int m_httpVersion;
    boost::asio::io_context m_ioc;
};


}
}
}
}

#endif /* BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_IMPL_ */
