#ifndef BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_IMPL_
#define BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_IMPL_

#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>

#include <chrono>
#include <cstdint>
#include <map>
#include <mutex>
#include <string>

#include "api/HttpClient.h"


namespace org {
namespace openapitools {
namespace client {
namespace api {


class HttpClientImpl : public HttpClient {

public:
    enum class Transport {
        Http,
        Https
    };

    // This overload preserves the original HTTP-only API. New code should
    // select the transport explicitly with the overload below.
    HttpClientImpl(const std::string &host,
                   const std::string &port,
                   const int httpVersion = 11); // HTTP 1.1

    HttpClientImpl(
        const std::string &host,
        const std::string &port,
        const Transport transport,
        const int httpVersion = 11,
        const std::chrono::milliseconds operationTimeout =
            std::chrono::milliseconds(30000),
        const std::uint64_t responseBodyLimit =
            8ULL * 1024ULL * 1024ULL);

    virtual ~HttpClientImpl() = default;

    std::pair<boost::beast::http::status, std::string>
    execute(const std::string &verb, const std::string &target,
            const std::string &body,
            const std::map<std::string, std::string> &headers) override;

    boost::beast::http::status
    executeStream(const std::string &verb, const std::string &target,
                  const std::string &body,
                  const std::map<std::string, std::string> &headers,
                  std::function<void(const std::string &)> onEvent) override;

protected:
    using HttpRequest =
        boost::beast::http::request<boost::beast::http::string_body>;
    using HttpResponse =
        std::pair<boost::beast::http::status, std::string>;
    using TlsStream =
        boost::asio::ssl::stream<boost::beast::tcp_stream>;

    virtual HttpResponse executeHttpRequest(HttpRequest &request);

    virtual HttpResponse executeHttpsRequest(HttpRequest &request);

    virtual boost::asio::ip::tcp::resolver::results_type resolveHost();

    virtual boost::asio::ip::tcp::socket connectSocket();

    virtual HttpRequest
    prepareRequest(const std::string &verb, const std::string &target,
                   const std::string &body,
                   const std::map<std::string, std::string> &headers);

    virtual void sendRequest(
        boost::asio::ip::tcp::socket &socket,
        HttpRequest &request);

    virtual HttpResponse
    receiveResponse(boost::asio::ip::tcp::socket &socket);

    virtual void closeSocket(boost::asio::ip::tcp::socket &socket);

    virtual void configureTlsContext(boost::asio::ssl::context &tlsContext);

private:
    boost::beast::http::status
    executeHttpStream(
        HttpRequest &request,
        std::function<void(const std::string &)> onEvent);

    boost::beast::http::status
    executeHttpsStream(
        HttpRequest &request,
        std::function<void(const std::string &)> onEvent);

    const std::string m_host;
    const std::string m_port;
    const int m_httpVersion;
    const Transport m_transport;
    const std::chrono::milliseconds m_operationTimeout;
    const std::uint64_t m_responseBodyLimit;
    boost::asio::io_context m_ioc;
    std::mutex m_executeMutex;
};


}
}
}
}

#endif /* BOOST_BEAST_OPENAPI_CLIENT_HTTP_CLIENT_IMPL_ */
