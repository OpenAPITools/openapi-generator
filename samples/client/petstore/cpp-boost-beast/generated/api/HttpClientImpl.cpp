
#include <boost/asio.hpp>
#include <boost/asio/connect.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/http/message.hpp>
#include <boost/beast/version.hpp>
#include <boost/system/system_error.hpp>

#include <sstream>
#include <string>

#include "api/HttpClientImpl.h"


namespace org {
namespace openapitools {
namespace client {
namespace api {

HttpClientImpl::HttpClientImpl(const std::string &host,
                               const std::string &port,
                               const int httpVersion /* = 11 */)
  : m_host(host), m_port(port), m_httpVersion(httpVersion)
{
}

std::pair<boost::beast::http::status, std::string>
HttpClientImpl::execute(const std::string &verb,
                        const std::string &target,
                        const std::string &body,
                        const std::map<std::string, std::string> headers) {

    boost::asio::ip::tcp::socket socket = connectSocket();

    boost::beast::http::request<boost::beast::http::string_body> req =
      prepareRequest(verb, target, body, headers);
    sendRequest(socket, req);

    const auto response = receiveResponse(socket);

    closeSocket(socket);
    return response;
}

boost::asio::ip::tcp::socket HttpClientImpl::connectSocket() {
    boost::asio::ip::tcp::resolver resolver(m_ioc);
    auto const results = resolver.resolve(m_host, m_port);

    boost::asio::ip::tcp::socket socket(m_ioc);
    boost::asio::connect(socket, results);
    return socket;
}

boost::beast::http::request<boost::beast::http::string_body>
HttpClientImpl::prepareRequest(const std::string &verb,
                               const std::string &target,
                               const std::string &body,
                               const std::map<std::string, std::string> headers) {
    boost::beast::http::request<boost::beast::http::string_body> req{
        boost::beast::http::string_to_verb(verb), target, m_httpVersion};

    req.set(boost::beast::http::field::host, m_host + ":" + m_port);
    req.body() = body;
    req.set(boost::beast::http::field::user_agent, BOOST_BEAST_VERSION_STRING);
    for (const auto & header : headers) {
        req.set(header.first, header.second);
    }

    req.prepare_payload();

    return req;
}

void HttpClientImpl::sendRequest(
    boost::asio::ip::tcp::socket &socket,
    boost::beast::http::request<boost::beast::http::string_body> &request) {
    boost::beast::http::write(socket, request);
}

std::pair<boost::beast::http::status, std::string>
HttpClientImpl::receiveResponse(boost::asio::ip::tcp::socket &socket) {
    boost::beast::flat_buffer buffer;
    boost::beast::http::response<boost::beast::http::dynamic_body> res;

    boost::beast::http::read(socket, buffer, res);

    const auto status = res.result();
    std::string body { boost::asio::buffers_begin(res.body().data()),
                       boost::asio::buffers_end(res.body().data()) };

    return std::pair<boost::beast::http::status, std::string>(
        status,
        body);
}

void HttpClientImpl::closeSocket(boost::asio::ip::tcp::socket &socket) {
    boost::beast::error_code ec;
    socket.shutdown(boost::asio::ip::tcp::socket::shutdown_both, ec);

    // not_connected happens sometimes so don't bother reporting it.
    if (ec && ec != boost::beast::errc::not_connected)
      throw boost::system::system_error(ec);
}


}
}
}
}
