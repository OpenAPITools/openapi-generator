
#include <boost/asio.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/http/message.hpp>
#include <boost/beast/version.hpp>
#include <boost/json/src.hpp>
#include <boost/system/system_error.hpp>

#include <openssl/err.h>
#include <openssl/ssl.h>

#include <chrono>
#include <functional>
#include <stdexcept>
#include <string>
#include <utility>

#include "api/HttpClientImpl.h"


namespace {

using OperationCompletion =
    std::function<void(const boost::beast::error_code &)>;

bool isAsciiAlphaNumeric(const unsigned char character) {
    return (character >= '0' && character <= '9') ||
        (character >= 'A' && character <= 'Z') ||
        (character >= 'a' && character <= 'z');
}

bool isRfcTokenCharacter(const unsigned char character) {
    if (isAsciiAlphaNumeric(character)) {
        return true;
    }

    switch (character) {
    case '!':
    case '#':
    case '$':
    case '%':
    case '&':
    case '\'':
    case '*':
    case '+':
    case '-':
    case '.':
    case '^':
    case '_':
    case '`':
    case '|':
    case '~':
        return true;
    default:
        return false;
    }
}

bool isRfcToken(const std::string &token) {
    if (token.empty()) {
        return false;
    }

    for (const unsigned char character : token) {
        if (!isRfcTokenCharacter(character)) {
            return false;
        }
    }
    return true;
}

bool containsControlCharacter(const std::string &value) {
    for (const unsigned char character : value) {
        if (character < 0x20 || character == 0x7f) {
            return true;
        }
    }
    return false;
}

bool isHexDigit(const unsigned char character) {
    return (character >= '0' && character <= '9') ||
        (character >= 'A' && character <= 'F') ||
        (character >= 'a' && character <= 'f');
}

bool isOriginFormTargetCharacter(const unsigned char character) {
    if (isAsciiAlphaNumeric(character)) {
        return true;
    }

    switch (character) {
    case '!':
    case '$':
    case '&':
    case '\'':
    case '(':
    case ')':
    case '*':
    case '+':
    case ',':
    case '-':
    case '.':
    case '/':
    case ':':
    case ';':
    case '=':
    case '?':
    case '@':
    case '_':
    case '~':
        return true;
    default:
        return false;
    }
}

void validateHost(const std::string &host) {
    if (host.empty()) {
        throw std::invalid_argument("host must not be empty");
    }
    if (containsControlCharacter(host) ||
        host.find_first_of(" /\\?#") != std::string::npos) {
        throw std::invalid_argument("host contains an invalid character");
    }
}

void validatePort(const std::string &port) {
    if (port.empty()) {
        throw std::invalid_argument("port must not be empty");
    }
    if (containsControlCharacter(port) ||
        port.find_first_of(" /\\:?#") != std::string::npos) {
        throw std::invalid_argument("port contains an invalid character");
    }
}

void validateRequestTarget(const std::string &target) {
    if (target.empty() || target.front() != '/') {
        throw std::invalid_argument("target must use HTTP origin-form");
    }

    for (std::size_t index = 0; index < target.size(); ++index) {
        const unsigned char character =
            static_cast<unsigned char>(target[index]);
        if (character == '%') {
            if (index + 2 >= target.size() ||
                !isHexDigit(static_cast<unsigned char>(target[index + 1])) ||
                !isHexDigit(static_cast<unsigned char>(target[index + 2]))) {
                throw std::invalid_argument("target contains an invalid percent escape");
            }
            index += 2;
        } else if (!isOriginFormTargetCharacter(character)) {
            throw std::invalid_argument("target contains an invalid character");
        }
    }
}

bool asciiCaseInsensitiveEqual(const std::string &headerName,
                               const char *reservedHeaderName) {
    const std::size_t reservedHeaderNameLength =
        std::char_traits<char>::length(reservedHeaderName);
    if (headerName.size() != reservedHeaderNameLength) {
        return false;
    }

    for (std::size_t index = 0; index < headerName.size(); ++index) {
        const unsigned char headerCharacter =
            static_cast<unsigned char>(headerName[index]);
        const unsigned char reservedCharacter =
            static_cast<unsigned char>(reservedHeaderName[index]);
        const unsigned char lowerHeaderCharacter =
            headerCharacter >= 'A' && headerCharacter <= 'Z'
            ? static_cast<unsigned char>(headerCharacter + ('a' - 'A'))
            : headerCharacter;
        const unsigned char lowerReservedCharacter =
            reservedCharacter >= 'A' && reservedCharacter <= 'Z'
            ? static_cast<unsigned char>(reservedCharacter + ('a' - 'A'))
            : reservedCharacter;
        if (lowerHeaderCharacter != lowerReservedCharacter) {
            return false;
        }
    }
    return true;
}

void validateCallerHeader(const std::string &headerName,
                          const std::string &headerValue) {
    if (!isRfcToken(headerName)) {
        throw std::invalid_argument("header name must be a non-empty RFC token");
    }

    const char *const reservedHeaderNames[] = {
        "Host",
        "Content-Length",
        "Transfer-Encoding",
        "Connection",
        "Proxy-Connection",
        "Keep-Alive",
        "Upgrade",
        "TE",
        "Trailer",
        "Expect",
        "Proxy-Authorization"};
    for (const char *reservedHeaderName : reservedHeaderNames) {
        if (asciiCaseInsensitiveEqual(headerName, reservedHeaderName)) {
            throw std::invalid_argument("header name is reserved by the transport");
        }
    }

    if (containsControlCharacter(headerValue)) {
        throw std::invalid_argument("header value must not contain control characters");
    }
}

void throwOperationError(const boost::beast::error_code &operationError,
                         const char *operationName) {
    if (operationError) {
        throw boost::system::system_error(operationError, operationName);
    }
}

void throwTimeout(const char *operationName) {
    const boost::beast::error_code timeoutError = boost::beast::error::timeout;
    throw boost::system::system_error(timeoutError, operationName);
}

void throwOpenSslError(const char *operationName) {
    const unsigned long openSslErrorValue = ::ERR_get_error();
    ::ERR_clear_error();
    if (openSslErrorValue == 0) {
        throw std::runtime_error(
            std::string(operationName) +
            " failed without an OpenSSL error code");
    }

    const boost::beast::error_code openSslError(
        static_cast<int>(openSslErrorValue),
        boost::asio::error::get_ssl_category());
    throw boost::system::system_error(openSslError, operationName);
}

template <typename InitiateOperation>
boost::beast::error_code runAsyncOperation(
    boost::asio::io_context &ioContext,
    const char *operationName,
    InitiateOperation initiateOperation) {
    boost::beast::error_code operationError;
    bool operationCompleted = false;

    ioContext.restart();
    initiateOperation(OperationCompletion(
        [&operationError, &operationCompleted](
            const boost::beast::error_code &completionError) {
            operationError = completionError;
            operationCompleted = true;
        }));
    ioContext.run();

    if (!operationCompleted) {
        throw std::runtime_error(
            std::string(operationName) + " did not invoke its completion handler");
    }

    return operationError;
}

template <typename InitiateOperation, typename CancelOperation>
boost::beast::error_code runTimedOperation(
    boost::asio::io_context &ioContext,
    const std::chrono::milliseconds operationTimeout,
    const char *operationName,
    InitiateOperation initiateOperation,
    CancelOperation cancelOperation) {
    boost::asio::steady_timer deadlineTimer(ioContext);
    boost::beast::error_code operationError;
    boost::beast::error_code timerError;
    bool operationCompleted = false;
    bool timeoutExpired = false;

    ioContext.restart();
    deadlineTimer.expires_after(operationTimeout);
    deadlineTimer.async_wait(
        [&operationCompleted, &timeoutExpired, &timerError, cancelOperation](
            const boost::beast::error_code &deadlineError) {
            if (deadlineError == boost::asio::error::operation_aborted ||
                operationCompleted) {
                return;
            }

            timerError = deadlineError;
            timeoutExpired = !deadlineError;
            cancelOperation();
        });

    initiateOperation(OperationCompletion(
        [&deadlineTimer, &operationError, &operationCompleted](
            const boost::beast::error_code &completionError) {
            operationError = completionError;
            operationCompleted = true;
            deadlineTimer.cancel();
        }));

    ioContext.run();

    if (timeoutExpired) {
        throwTimeout(operationName);
    }
    throwOperationError(timerError, operationName);
    if (!operationCompleted) {
        throw std::runtime_error(
            std::string(operationName) + " did not invoke its completion handler");
    }

    return operationError;
}

} // namespace


namespace org {
namespace openapitools {
namespace client {
namespace api {

HttpClientImpl::HttpClientImpl(const std::string &host,
                               const std::string &port,
                               const int httpVersion /* = 11 */)
  : HttpClientImpl(host,
                   port,
                   Transport::Http,
                   httpVersion,
                   std::chrono::milliseconds(30000),
                   8ULL * 1024ULL * 1024ULL)
{
}

HttpClientImpl::HttpClientImpl(
    const std::string &host,
    const std::string &port,
    const Transport transport,
    const int httpVersion /* = 11 */,
    const std::chrono::milliseconds operationTimeout /* = 30000ms */,
    const std::uint64_t responseBodyLimit /* = 8 MiB */)
  : m_host(host),
    m_port(port),
    m_httpVersion(httpVersion),
    m_transport(transport),
    m_operationTimeout(operationTimeout),
    m_responseBodyLimit(responseBodyLimit)
{
    validateHost(m_host);
    validatePort(m_port);
    if (m_httpVersion != 10 && m_httpVersion != 11) {
        throw std::invalid_argument("httpVersion must be 10 or 11");
    }
    if (m_transport != Transport::Http && m_transport != Transport::Https) {
        throw std::invalid_argument("transport must be HTTP or HTTPS");
    }
    if (m_operationTimeout <= std::chrono::milliseconds::zero()) {
        throw std::invalid_argument("operationTimeout must be greater than zero");
    }
}

std::pair<boost::beast::http::status, std::string>
HttpClientImpl::execute(const std::string &verb,
                        const std::string &target,
                        const std::string &body,
                        const std::map<std::string, std::string> &headers) {
    const std::lock_guard<std::mutex> executeLock(m_executeMutex);
    HttpRequest request = prepareRequest(verb, target, body, headers);

    if (m_transport == Transport::Https) {
        return executeHttpsRequest(request);
    }

    return executeHttpRequest(request);
}

HttpClientImpl::HttpResponse
HttpClientImpl::executeHttpRequest(HttpRequest &request) {
    boost::asio::ip::tcp::socket socket = connectSocket();
    sendRequest(socket, request);

    const auto response = receiveResponse(socket);
    closeSocket(socket);
    return response;
}

HttpClientImpl::HttpResponse
HttpClientImpl::executeHttpsRequest(HttpRequest &request) {
    boost::asio::ssl::context tlsContext(boost::asio::ssl::context::tls_client);
    configureTlsContext(tlsContext);

    TlsStream tlsStream(m_ioc, tlsContext);
    boost::beast::error_code hostAddressError;
    const boost::asio::ip::address hostAddress =
        boost::asio::ip::make_address(m_host, hostAddressError);
    const bool hostIsIpAddress =
        !hostAddressError && (hostAddress.is_v4() || hostAddress.is_v6());
    if (!hostIsIpAddress) {
        ::ERR_clear_error();
        if (!SSL_set_tlsext_host_name(
                tlsStream.native_handle(), m_host.c_str())) {
            throwOpenSslError("set TLS SNI hostname");
        }
    }
    tlsStream.set_verify_callback(
        boost::asio::ssl::host_name_verification(m_host));

    const auto resolvedEndpoints = resolveHost();
    boost::beast::tcp_stream &tcpStream =
        boost::beast::get_lowest_layer(tlsStream);

    tcpStream.expires_after(m_operationTimeout);
    throwOperationError(
        runAsyncOperation(
            m_ioc,
            "connect",
            [&tcpStream, &resolvedEndpoints](
                const OperationCompletion &operationCompletion) {
                tcpStream.async_connect(
                    resolvedEndpoints,
                    [operationCompletion](
                        const boost::beast::error_code &connectError,
                        const boost::asio::ip::tcp::endpoint &) {
                        operationCompletion(connectError);
                    });
            }),
        "connect");

    tcpStream.expires_after(m_operationTimeout);
    throwOperationError(
        runAsyncOperation(
            m_ioc,
            "TLS handshake",
            [&tlsStream](const OperationCompletion &operationCompletion) {
                tlsStream.async_handshake(
                    boost::asio::ssl::stream_base::client,
                    operationCompletion);
            }),
        "TLS handshake");

    tcpStream.expires_after(m_operationTimeout);
    throwOperationError(
        runAsyncOperation(
            m_ioc,
            "write",
            [&tlsStream, &request](
                const OperationCompletion &operationCompletion) {
                boost::beast::http::async_write(
                    tlsStream,
                    request,
                    [operationCompletion](
                        const boost::beast::error_code &writeError,
                        const std::size_t) {
                        operationCompletion(writeError);
                    });
            }),
        "write");

    boost::beast::flat_buffer responseBuffer;
    boost::beast::http::response_parser<boost::beast::http::dynamic_body>
        responseParser;
    responseParser.body_limit(m_responseBodyLimit);
    tcpStream.expires_after(m_operationTimeout);
    throwOperationError(
        runAsyncOperation(
            m_ioc,
            "read",
            [&tlsStream, &responseBuffer, &responseParser](
                const OperationCompletion &operationCompletion) {
                boost::beast::http::async_read(
                    tlsStream,
                    responseBuffer,
                    responseParser,
                    [operationCompletion](
                        const boost::beast::error_code &readError,
                        const std::size_t) {
                        operationCompletion(readError);
                    });
            }),
        "read");
    boost::beast::http::response<boost::beast::http::dynamic_body> response =
        responseParser.release();

    tcpStream.expires_after(m_operationTimeout);
    const boost::beast::error_code shutdownError = runAsyncOperation(
        m_ioc,
        "TLS shutdown",
        [&tlsStream](const OperationCompletion &operationCompletion) {
            tlsStream.async_shutdown(operationCompletion);
        });

    // Once Beast has parsed a complete HTTP message, a peer that omits
    // close_notify cannot truncate the accepted response body.
    if (shutdownError &&
        shutdownError != boost::asio::ssl::error::stream_truncated &&
        shutdownError != boost::asio::error::eof) {
        throwOperationError(shutdownError, "TLS shutdown");
    }

    const std::string responseBody(
        boost::asio::buffers_begin(response.body().data()),
        boost::asio::buffers_end(response.body().data()));
    return HttpResponse(response.result(), responseBody);
}

boost::asio::ip::tcp::resolver::results_type
HttpClientImpl::resolveHost() {
    boost::asio::ip::tcp::resolver resolver(m_ioc);
    boost::asio::ip::tcp::resolver::results_type resolvedEndpoints;

    throwOperationError(
        runTimedOperation(
            m_ioc,
            m_operationTimeout,
            "resolve",
            [this, &resolver, &resolvedEndpoints](
                const OperationCompletion &operationCompletion) {
                resolver.async_resolve(
                    m_host,
                    m_port,
                    [&resolvedEndpoints, operationCompletion](
                        const boost::beast::error_code &resolveError,
                        boost::asio::ip::tcp::resolver::results_type endpoints) {
                        if (!resolveError) {
                            resolvedEndpoints = std::move(endpoints);
                        }
                        operationCompletion(resolveError);
                    });
            },
            [&resolver]() {
                resolver.cancel();
            }),
        "resolve");

    return resolvedEndpoints;
}

boost::asio::ip::tcp::socket HttpClientImpl::connectSocket() {
    const auto resolvedEndpoints = resolveHost();
    boost::asio::ip::tcp::socket socket(m_ioc);

    throwOperationError(
        runTimedOperation(
            m_ioc,
            m_operationTimeout,
            "connect",
            [&socket, &resolvedEndpoints](
                const OperationCompletion &operationCompletion) {
                boost::asio::async_connect(
                    socket,
                    resolvedEndpoints,
                    [operationCompletion](
                        const boost::beast::error_code &connectError,
                        const boost::asio::ip::tcp::endpoint &) {
                        operationCompletion(connectError);
                    });
            },
            [&socket]() {
                boost::beast::error_code closeError;
                socket.close(closeError);
            }),
        "connect");

    return socket;
}

HttpClientImpl::HttpRequest
HttpClientImpl::prepareRequest(const std::string &verb,
                               const std::string &target,
                               const std::string &body,
                               const std::map<std::string, std::string> &headers) {
    if (!isRfcToken(verb)) {
        throw std::invalid_argument("verb must be a non-empty RFC token");
    }
    validateRequestTarget(target);

    HttpRequest request;
    request.version(m_httpVersion);
    request.method_string(verb);
    request.target(target);
    request.body() = body;

    boost::beast::error_code hostAddressError;
    const boost::asio::ip::address hostAddress =
        boost::asio::ip::make_address(m_host, hostAddressError);
    const std::string hostHeader = !hostAddressError && hostAddress.is_v6()
        ? "[" + m_host + "]:" + m_port
        : m_host + ":" + m_port;
    request.set(boost::beast::http::field::host, hostHeader);
    request.set(
        boost::beast::http::field::user_agent,
        BOOST_BEAST_VERSION_STRING);
    for (const auto &header : headers) {
        validateCallerHeader(header.first, header.second);
        request.set(header.first, header.second);
    }

    request.prepare_payload();

    return request;
}

void HttpClientImpl::sendRequest(
    boost::asio::ip::tcp::socket &socket,
    HttpRequest &request) {
    throwOperationError(
        runTimedOperation(
            m_ioc,
            m_operationTimeout,
            "write",
            [&socket, &request](
                const OperationCompletion &operationCompletion) {
                boost::beast::http::async_write(
                    socket,
                    request,
                    [operationCompletion](
                        const boost::beast::error_code &writeError,
                        const std::size_t) {
                        operationCompletion(writeError);
                    });
            },
            [&socket]() {
                boost::beast::error_code closeError;
                socket.close(closeError);
            }),
        "write");
}

HttpClientImpl::HttpResponse
HttpClientImpl::receiveResponse(boost::asio::ip::tcp::socket &socket) {
    boost::beast::flat_buffer responseBuffer;
    boost::beast::http::response_parser<boost::beast::http::dynamic_body>
        responseParser;
    responseParser.body_limit(m_responseBodyLimit);

    throwOperationError(
        runTimedOperation(
            m_ioc,
            m_operationTimeout,
            "read",
            [&socket, &responseBuffer, &responseParser](
                const OperationCompletion &operationCompletion) {
                boost::beast::http::async_read(
                    socket,
                    responseBuffer,
                    responseParser,
                    [operationCompletion](
                        const boost::beast::error_code &readError,
                        const std::size_t) {
                        operationCompletion(readError);
                    });
            },
            [&socket]() {
                boost::beast::error_code closeError;
                socket.close(closeError);
            }),
        "read");
    boost::beast::http::response<boost::beast::http::dynamic_body> response =
        responseParser.release();

    const std::string responseBody(
        boost::asio::buffers_begin(response.body().data()),
        boost::asio::buffers_end(response.body().data()));
    return HttpResponse(response.result(), responseBody);
}

void HttpClientImpl::closeSocket(boost::asio::ip::tcp::socket &socket) {
    boost::beast::error_code shutdownError;
    socket.shutdown(
        boost::asio::ip::tcp::socket::shutdown_both,
        shutdownError);

    // not_connected happens sometimes so don't bother reporting it.
    if (shutdownError && shutdownError != boost::beast::errc::not_connected) {
        throw boost::system::system_error(shutdownError);
    }
}

void HttpClientImpl::configureTlsContext(
    boost::asio::ssl::context &tlsContext) {
    ::ERR_clear_error();
    if (SSL_CTX_set_min_proto_version(
            tlsContext.native_handle(), TLS1_2_VERSION) != 1) {
        throwOpenSslError("set minimum TLS protocol version");
    }
    tlsContext.set_default_verify_paths();
    tlsContext.set_verify_mode(boost::asio::ssl::verify_peer);
}


}
}
}
}
