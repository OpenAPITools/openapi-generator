#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/system/system_error.hpp>
#include <boost/test/unit_test.hpp>

#include <openssl/ssl.h>

#include <array>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <exception>
#include <map>
#include <mutex>
#include <stdexcept>
#include <string>
#include <thread>
#include <utility>
#include <vector>

#include "api/HttpClientImpl.h"

namespace {

namespace asio = boost::asio;
namespace beast = boost::beast;
namespace http = boost::beast::http;
namespace ssl = boost::asio::ssl;
using tcp = boost::asio::ip::tcp;
using org::openapitools::client::api::HttpClient;
using org::openapitools::client::api::HttpClientImpl;

class BufferedOnlyHttpClient final : public HttpClient {
public:
    std::pair<http::status, std::string> execute(
        const std::string &,
        const std::string &,
        const std::string &,
        const std::map<std::string, std::string> &) override {
        return {http::status::ok, "buffered"};
    }
};

const char testServerCertificatePem[] = R"PEM(-----BEGIN CERTIFICATE-----
MIIDjzCCAnegAwIBAgIUapcP6xBH1z5Lnub2alXHWLwLABwwDQYJKoZIhvcNAQEL
BQAwNjESMBAGA1UEAwwJbG9jYWxob3N0MSAwHgYDVQQKDBdPcGVuQVBJIEdlbmVy
YXRvciBUZXN0czAgFw0yNjA3MTcxNjI1NTRaGA8yMTI2MDYyMzE2MjU1NFowNjES
MBAGA1UEAwwJbG9jYWxob3N0MSAwHgYDVQQKDBdPcGVuQVBJIEdlbmVyYXRvciBU
ZXN0czCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAIG/uoJQ+upnmBZI
d8W9Hy9m/FBxnODgFcmV9V6IytndB0/97sgdPDVm1DcGi7cvV3avk9eiMZl4ONX7
4AJkrZ0+hsiZzhjSgNO9LiN+9OoFQZ+DJ2qrJVrfGMzjFfMrDu585pex96JKefUn
CV4zA2Z2iHPZq7Z0r/KS+WxaqF5swzqSmbKQ00jWQsS8DC8qjqbtWY+ShPwRv/iN
r8OxHI5IF5J2a0x47wUVrnVAI9yB7IsQ/Rcsyw8aAltAl2Yp1hRXXjtYXYluls6t
Eo5eSB7Zz08hZ3ZQQQbqbZzmCbGokoMGw2YTol+ggK07ULuCo9MT+GNG/8cmYJ0Y
hMenPF8CAwEAAaOBkjCBjzAdBgNVHQ4EFgQUh0N9XEtsfGFMMV/0P0wbAkOI5qUw
HwYDVR0jBBgwFoAUh0N9XEtsfGFMMV/0P0wbAkOI5qUwFAYDVR0RBA0wC4IJbG9j
YWxob3N0MBIGA1UdEwEB/wQIMAYBAf8CAQAwDgYDVR0PAQH/BAQDAgKkMBMGA1Ud
JQQMMAoGCCsGAQUFBwMBMA0GCSqGSIb3DQEBCwUAA4IBAQAHsx128vbhfChMRFhr
QCcYENrzm6Grb5uI6Ggid7aU38+9UPqcmEyBbJ5nvpLotbLqlCiAX6fHZAdhVbWI
8KaU0k4WoHg3oFnRT6742R7m2ldcffEJw+gBQgvDeOnFoHY92ycoYaPqICPZA0lT
El04GAfI/z6IWcSEDnoexlzCp/+bChfSSRhrp+94Oh21tn+POg8usC4eONh47mXq
GYSObucn03da19Vix/LRMYSDBVz78a4/6u868St7mlLMmH9QI0tnzpTNHiU252i5
XLGePaBC0U6ESMXpUv/h/KIKHQIVuCjYC6LpO5CRzCUVxEcic44nkeX2F5rHOoYz
pqt8
-----END CERTIFICATE-----
)PEM";

const char testServerPrivateKeyPem[] = R"PEM(-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCBv7qCUPrqZ5gW
SHfFvR8vZvxQcZzg4BXJlfVeiMrZ3QdP/e7IHTw1ZtQ3Bou3L1d2r5PXojGZeDjV
++ACZK2dPobImc4Y0oDTvS4jfvTqBUGfgydqqyVa3xjM4xXzKw7ufOaXsfeiSnn1
JwleMwNmdohz2au2dK/ykvlsWqhebMM6kpmykNNI1kLEvAwvKo6m7VmPkoT8Eb/4
ja/DsRyOSBeSdmtMeO8FFa51QCPcgeyLEP0XLMsPGgJbQJdmKdYUV147WF2JbpbO
rRKOXkge2c9PIWd2UEEG6m2c5gmxqJKDBsNmE6JfoICtO1C7gqPTE/hjRv/HJmCd
GITHpzxfAgMBAAECggEAKWYyrYgHN6nMRjfjN43LmIVktoo5wyrc6NBXVU2SZAd5
YNWmTeW+I0/nIJOPGukZID6Pjrsq1kwc3nIanMjg+I5N1U32C8eG4Yi4Yw0MOmto
7+uaNRfcuGz87cr7YvZ3MjNaJEbdu8PFvCozfAIM8LGbglOOzY3X4i9xNFiM5mub
ZP8w+LcSKf19GlXluTnp2oc9cHumPYjdGO5Xa7X3OngwESU+/njHuets0UgHrdpa
tPs+GR1XgpSMasJZ+xRYKjwo0YRXwop+M17R4soLVwJlrUNeVInZnn+tx+c0oICK
Bd/x7NQQA+Wa52EBPY8A1kbV0bCXjFCqpJIhpN5vkQKBgQC3OD87wjWEr6Byaqxq
xojUOVJkOuyD6TI9CquKEy/83LeLlmjuu6bZQ8fp9qu3HTMesuwSxa6WnjvZ1xHU
auQGO2XhFa47GOZKTZeUdPjSTcnlv4gvU4D5Dbmn2zvZpeAhVDqnyZ4EwYT9adYj
gVwYGE/TkMmRabNB8vquEQjCBwKBgQC1Sf9cOeI+ac/iAedgfGhY6Y7c/wlatcTH
VG6pJbVApQw/0h5nYKyYsNkjR0VKaK4zslclDYScuOaw9CRSdOvKdCXisHEnus3A
v0M9OC/M5VnZCbQI96DxHy/pf0AKOICV1NQd5zxllba/kb9Yp/lLD93pr2kbGxsP
naHSK+M86QKBgQC1AiKmfSW2YnQW1YBlNKL1V3QohXYok/gICkSScoZUQBItG3mr
cURTHP2iDQp0K6rXR1im6xmiv8zNWAbLXpbL+V6bUnoA9IZ/Hc0E49L2odc23VRD
vTGL251xUcv9KD0XcElKfotYk385hJgEF8bOHyauEzqwKEbqfoxd74JBfQKBgDRU
EK6pUqHbXxMIo6XZIzGO8RrfTu7tCsjSmUKfzMrFGV0OpJIIeKl2WqbxOpH411e6
wabDWST3xjcLsbRPZYRBtHHTpkXmiN97FciFAF8vUJjyURJJkB4pd2X5wh1xI8Sb
80JSbb+Q0HhJdKb9jVraUJ7UcA1P7Ka8RGprGqOJAoGBAJa2RSY/TDxNa2FpZykN
i/K/1huvNlrIc4A9Xm3GK3YtDamFyrpI3qR24ZHtBM6zq8XVQrXOXQQS8yYEJBcd
mHuETp7BnLt3awy9Iw+H6rolRI0xbbBPX3y5qh5ldgeyK8s7wHcEfez6tdljCd8n
uSrp28U4dFRKZfyy7UJoAV7o
-----END PRIVATE KEY-----
)PEM";

tcp::socket acceptLoopbackConnection(tcp::acceptor &acceptor,
                                     asio::io_context &serverIoContext) {
    acceptor.non_blocking(true);
    tcp::socket acceptedSocket(serverIoContext);
    const std::chrono::steady_clock::time_point acceptDeadline =
        std::chrono::steady_clock::now() + std::chrono::seconds(3);

    while (std::chrono::steady_clock::now() < acceptDeadline) {
        beast::error_code acceptError;
        acceptor.accept(acceptedSocket, acceptError);
        if (!acceptError) {
            return acceptedSocket;
        }
        if (acceptError != asio::error::would_block &&
            acceptError != asio::error::try_again) {
            throw boost::system::system_error(acceptError, "accept");
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }

    throw std::runtime_error("loopback server did not receive a connection");
}

void closeTcpSendWithoutTlsCloseNotify(ssl::stream<tcp::socket> &tlsStream) {
    beast::error_code shutdownSendError;
    tlsStream.next_layer().shutdown(tcp::socket::shutdown_send,
                                    shutdownSendError);
    if (shutdownSendError && shutdownSendError != beast::errc::not_connected) {
        throw boost::system::system_error(shutdownSendError,
                                          "TCP send shutdown");
    }

    beast::error_code nonBlockingError;
    tlsStream.next_layer().non_blocking(true, nonBlockingError);
    if (nonBlockingError) {
        throw boost::system::system_error(nonBlockingError,
                                          "set non-blocking drain");
    }

    std::array<char, 512> incomingTlsBytes;
    const std::chrono::steady_clock::time_point drainDeadline =
        std::chrono::steady_clock::now() + std::chrono::seconds(2);
    while (std::chrono::steady_clock::now() < drainDeadline) {
        beast::error_code drainError;
        tlsStream.next_layer().read_some(asio::buffer(incomingTlsBytes),
                                         drainError);
        if (!drainError) {
            continue;
        }
        if (drainError == asio::error::eof ||
            drainError == asio::error::connection_reset) {
            return;
        }
        if (drainError != asio::error::would_block &&
            drainError != asio::error::try_again) {
            throw boost::system::system_error(drainError, "drain TLS peer");
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }

    throw std::runtime_error("TLS peer did not close after TCP half-close");
}

class TrustedTestHttpClient final : public HttpClientImpl {
public:
    TrustedTestHttpClient(const std::string &host,
                          const std::string &port,
                          const std::chrono::milliseconds operationTimeout,
                          const std::uint64_t responseBodyLimit =
                              8ULL * 1024ULL * 1024ULL)
        : HttpClientImpl(host,
                         port,
                         Transport::Https,
                         11,
                         operationTimeout,
                         responseBodyLimit) {}

protected:
    void configureTlsContext(ssl::context &tlsContext) override {
        HttpClientImpl::configureTlsContext(tlsContext);
        tlsContext.add_certificate_authority(asio::buffer(
            testServerCertificatePem, sizeof(testServerCertificatePem) - 1));
    }
};

class RequestInspectingHttpClient final : public HttpClientImpl {
public:
    RequestInspectingHttpClient(const std::string &host,
                                const std::string &port)
        : HttpClientImpl(host, port) {}

    std::string prepareHostHeader(
        const std::map<std::string, std::string> &headers) {
        const HttpRequest request = prepareRequest("GET", "/", "", headers);
        const beast::string_view hostHeader = request[http::field::host];
        return std::string(hostHeader.data(), hostHeader.size());
    }
};

class SerializedLifecycleHttpClient final : public HttpClientImpl {
public:
    SerializedLifecycleHttpClient()
        : HttpClientImpl("localhost", "80"),
          firstExecutionActive_(false),
          secondCallStarted_(false),
          secondCallObserved_(false),
          prepareOverlappedFirstExecution_(false),
          executeHookCallCount_(0) {}

    bool waitForFirstExecution() {
        std::unique_lock<std::mutex> stateLock(stateMutex_);
        return stateCondition_.wait_for(
            stateLock,
            std::chrono::seconds(1),
            [this]() { return firstExecutionActive_; });
    }

    void markSecondCallStarted() {
        {
            std::lock_guard<std::mutex> stateLock(stateMutex_);
            secondCallStarted_ = true;
        }
        stateCondition_.notify_all();
    }

    bool secondCallObserved() const {
        std::lock_guard<std::mutex> stateLock(stateMutex_);
        return secondCallObserved_;
    }

    bool prepareOverlappedFirstExecution() const {
        std::lock_guard<std::mutex> stateLock(stateMutex_);
        return prepareOverlappedFirstExecution_;
    }

    std::size_t executeHookCallCount() const {
        std::lock_guard<std::mutex> stateLock(stateMutex_);
        return executeHookCallCount_;
    }

protected:
    HttpRequest prepareRequest(
        const std::string &verb,
        const std::string &target,
        const std::string &body,
        const std::map<std::string, std::string> &headers) override {
        bool overlapDetected = false;
        {
            std::lock_guard<std::mutex> stateLock(stateMutex_);
            if (firstExecutionActive_) {
                prepareOverlappedFirstExecution_ = true;
                overlapDetected = true;
            }
        }
        if (overlapDetected) {
            stateCondition_.notify_all();
        }
        return HttpClientImpl::prepareRequest(verb, target, body, headers);
    }

    HttpResponse executeHttpRequest(HttpRequest &) override {
        bool isFirstExecution = false;
        {
            std::unique_lock<std::mutex> stateLock(stateMutex_);
            ++executeHookCallCount_;
            isFirstExecution = executeHookCallCount_ == 1;
            if (isFirstExecution) {
                firstExecutionActive_ = true;
                stateCondition_.notify_all();
                secondCallObserved_ = stateCondition_.wait_for(
                    stateLock,
                    std::chrono::seconds(1),
                    [this]() { return secondCallStarted_; });
                if (secondCallObserved_) {
                    stateCondition_.wait_for(
                        stateLock,
                        std::chrono::milliseconds(500),
                        [this]() {
                            return prepareOverlappedFirstExecution_;
                        });
                }
                firstExecutionActive_ = false;
            }
        }

        if (isFirstExecution) {
            stateCondition_.notify_all();
        }

        return HttpResponse(http::status::ok, "serialized response");
    }

private:
    mutable std::mutex stateMutex_;
    std::condition_variable stateCondition_;
    bool firstExecutionActive_;
    bool secondCallStarted_;
    bool secondCallObserved_;
    bool prepareOverlappedFirstExecution_;
    std::size_t executeHookCallCount_;
};

class LoopbackTlsServer final {
public:
    enum class Exchange {
        SendResponse,
        ExpectRejectedHandshake
    };

    explicit LoopbackTlsServer(
        const Exchange expectedExchange,
        std::string responseBody = "secure loopback response")
        : serverTlsContext_(ssl::context::tls_server),
          acceptor_(serverIoContext_,
                     tcp::endpoint(asio::ip::address_v4::loopback(), 0)),
          expectedExchange_(expectedExchange),
          responseBody_(std::move(responseBody)),
          handshakeRejected_(false),
          responseSent_(false) {
        serverTlsContext_.use_certificate_chain(asio::buffer(
            testServerCertificatePem, sizeof(testServerCertificatePem) - 1));
        serverTlsContext_.use_private_key(
            asio::buffer(testServerPrivateKeyPem,
                         sizeof(testServerPrivateKeyPem) - 1),
            ssl::context::pem);
        serverThread_ = std::thread(&LoopbackTlsServer::serve, this);
    }

    ~LoopbackTlsServer() {
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
    }

    LoopbackTlsServer(const LoopbackTlsServer &) = delete;
    LoopbackTlsServer &operator=(const LoopbackTlsServer &) = delete;

    std::string port() const {
        return std::to_string(acceptor_.local_endpoint().port());
    }

    void waitForCompletion() {
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
        if (serverException_) {
            std::rethrow_exception(serverException_);
        }
    }

    const std::string &sniHostname() const {
        return sniHostname_;
    }

    const std::string &requestTarget() const {
        return requestTarget_;
    }

    bool handshakeRejected() const {
        return handshakeRejected_;
    }

    bool responseSent() const {
        return responseSent_;
    }

private:
    void serve() {
        try {
            tcp::socket acceptedSocket =
                acceptLoopbackConnection(acceptor_, serverIoContext_);
            ssl::stream<tcp::socket> tlsStream(std::move(acceptedSocket),
                                               serverTlsContext_);

            beast::error_code handshakeError;
            tlsStream.handshake(ssl::stream_base::server, handshakeError);
            const char *const receivedSniHostname = SSL_get_servername(
                tlsStream.native_handle(), TLSEXT_NAMETYPE_host_name);
            if (receivedSniHostname != nullptr) {
                sniHostname_ = receivedSniHostname;
            }
            if (handshakeError) {
                handshakeRejected_ = true;
                if (expectedExchange_ == Exchange::SendResponse) {
                    throw boost::system::system_error(handshakeError,
                                                      "TLS handshake");
                }
                return;
            }

            if (expectedExchange_ == Exchange::ExpectRejectedHandshake) {
                return;
            }

            beast::flat_buffer requestBuffer;
            http::request<http::string_body> receivedRequest;
            http::read(tlsStream, requestBuffer, receivedRequest);
            requestTarget_ = std::string(receivedRequest.target());

            http::response<http::string_body> serverResponse{
                http::status::ok, receivedRequest.version()};
            serverResponse.set(http::field::content_type, "text/plain");
            serverResponse.keep_alive(false);
            serverResponse.body() = responseBody_;
            serverResponse.prepare_payload();
            http::write(tlsStream, serverResponse);
            responseSent_ = true;

            closeTcpSendWithoutTlsCloseNotify(tlsStream);
        } catch (...) {
            serverException_ = std::current_exception();
        }
    }

    asio::io_context serverIoContext_;
    ssl::context serverTlsContext_;
    tcp::acceptor acceptor_;
    Exchange expectedExchange_;
    std::string responseBody_;
    std::thread serverThread_;
    std::exception_ptr serverException_;
    std::string sniHostname_;
    std::string requestTarget_;
    bool handshakeRejected_;
    bool responseSent_;
};

class PlainStallServer final {
public:
    PlainStallServer()
        : acceptor_(serverIoContext_,
                    tcp::endpoint(asio::ip::address_v4::loopback(), 0)),
          stopRequested_(false),
          connectionAccepted_(false),
          serverThread_(&PlainStallServer::serve, this) {}

    ~PlainStallServer() {
        requestStop();
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
    }

    PlainStallServer(const PlainStallServer &) = delete;
    PlainStallServer &operator=(const PlainStallServer &) = delete;

    std::string port() const {
        return std::to_string(acceptor_.local_endpoint().port());
    }

    void stopAndWait() {
        requestStop();
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
        if (serverException_) {
            std::rethrow_exception(serverException_);
        }
    }

    bool connectionAccepted() const {
        return connectionAccepted_;
    }

private:
    void requestStop() {
        {
            std::lock_guard<std::mutex> lock(stopMutex_);
            stopRequested_ = true;
        }
        stopCondition_.notify_all();
    }

    void serve() {
        try {
            tcp::socket acceptedSocket =
                acceptLoopbackConnection(acceptor_, serverIoContext_);
            {
                std::unique_lock<std::mutex> lock(stopMutex_);
                connectionAccepted_ = true;
                stopCondition_.wait_for(
                    lock,
                    std::chrono::seconds(2),
                    [this]() { return stopRequested_; });
            }
        } catch (...) {
            serverException_ = std::current_exception();
        }
    }

    asio::io_context serverIoContext_;
    tcp::acceptor acceptor_;
    mutable std::mutex stopMutex_;
    std::condition_variable stopCondition_;
    bool stopRequested_;
    bool connectionAccepted_;
    std::exception_ptr serverException_;
    std::thread serverThread_;
};

class PlainResponseServer final {
public:
    explicit PlainResponseServer(const std::string &responseBody)
        : acceptor_(serverIoContext_,
                    tcp::endpoint(asio::ip::address_v4::loopback(), 0)),
          responseBody_(responseBody),
          serverThread_(&PlainResponseServer::serve, this) {}

    ~PlainResponseServer() {
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
    }

    PlainResponseServer(const PlainResponseServer &) = delete;
    PlainResponseServer &operator=(const PlainResponseServer &) = delete;

    std::string port() const {
        return std::to_string(acceptor_.local_endpoint().port());
    }

    void waitForCompletion() {
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
        if (serverException_) {
            std::rethrow_exception(serverException_);
        }
    }

private:
    void serve() {
        try {
            tcp::socket acceptedSocket =
                acceptLoopbackConnection(acceptor_, serverIoContext_);
            http::response<http::string_body> serverResponse{
                http::status::ok, 11};
            serverResponse.keep_alive(false);
            serverResponse.body() = responseBody_;
            serverResponse.prepare_payload();

            beast::error_code writeError;
            http::write(acceptedSocket, serverResponse, writeError);
            if (writeError && writeError != asio::error::broken_pipe &&
                writeError != asio::error::connection_reset) {
                throw boost::system::system_error(writeError,
                                                  "write HTTP response");
            }
        } catch (...) {
            serverException_ = std::current_exception();
        }
    }

    asio::io_context serverIoContext_;
    tcp::acceptor acceptor_;
    std::string responseBody_;
    std::exception_ptr serverException_;
    std::thread serverThread_;
};

boost::system::error_code captureExecutionError(HttpClientImpl &httpClient,
                                                const std::string &target) {
    try {
        httpClient.execute("GET", target, "", {});
    } catch (const boost::system::system_error &executionException) {
        return executionException.code();
    }
    return {};
}

} // namespace

BOOST_AUTO_TEST_SUITE(HttpClientBehaviorTest)

BOOST_AUTO_TEST_CASE(buffered_only_adapter_inherits_streaming_fallback) {
    BufferedOnlyHttpClient httpClient;
    BOOST_REQUIRE_THROW(
        httpClient.executeStream("GET", "/stream", "", {}, [](const std::string &) {}),
        std::logic_error);
}

BOOST_AUTO_TEST_CASE(shared_client_execute_calls_are_serialized) {
    SerializedLifecycleHttpClient httpClient;
    std::exception_ptr firstExecutionException;
    std::exception_ptr secondExecutionException;

    std::thread firstExecutionThread([&httpClient, &firstExecutionException]() {
        try {
            httpClient.execute("GET", "/first", "", {});
        } catch (...) {
            firstExecutionException = std::current_exception();
        }
    });

    const bool firstExecutionEntered = httpClient.waitForFirstExecution();
    std::thread secondExecutionThread(
        [&httpClient, &secondExecutionException]() {
            httpClient.markSecondCallStarted();
            try {
                httpClient.execute("GET", "/second", "", {});
            } catch (...) {
                secondExecutionException = std::current_exception();
            }
        });

    firstExecutionThread.join();
    secondExecutionThread.join();

    if (firstExecutionException) {
        std::rethrow_exception(firstExecutionException);
    }
    if (secondExecutionException) {
        std::rethrow_exception(secondExecutionException);
    }
    BOOST_REQUIRE(firstExecutionEntered);
    BOOST_REQUIRE(httpClient.secondCallObserved());
    BOOST_REQUIRE(!httpClient.prepareOverlappedFirstExecution());
    BOOST_REQUIRE_EQUAL(httpClient.executeHookCallCount(), 2U);
}

BOOST_AUTO_TEST_CASE(prepare_request_formats_host_for_address_type) {
    RequestInspectingHttpClient dnsClient("example.test", "8080");
    RequestInspectingHttpClient ipv4Client("192.0.2.10", "8081");
    RequestInspectingHttpClient ipv6Client("2001:db8::10", "8082");

    BOOST_REQUIRE_EQUAL(dnsClient.prepareHostHeader({}),
                        "example.test:8080");
    BOOST_REQUIRE_EQUAL(ipv4Client.prepareHostHeader({}),
                        "192.0.2.10:8081");
    BOOST_REQUIRE_EQUAL(ipv6Client.prepareHostHeader({}),
                        "[2001:db8::10]:8082");

    const std::map<std::string, std::string> overridingHeaders{
        {"Host", "caller.example:9090"}};
    BOOST_REQUIRE_EQUAL(ipv6Client.prepareHostHeader(overridingHeaders),
                        "caller.example:9090");
}

BOOST_AUTO_TEST_CASE(
    trusted_https_accepts_response_without_close_notify_and_sends_sni) {
    LoopbackTlsServer tlsServer(LoopbackTlsServer::Exchange::SendResponse);
    TrustedTestHttpClient httpClient(
        "localhost", tlsServer.port(), std::chrono::milliseconds(1000));

    const std::pair<http::status, std::string> httpResponse =
        httpClient.execute("GET", "/secure", "", {});
    tlsServer.waitForCompletion();

    BOOST_REQUIRE_EQUAL(httpResponse.first, http::status::ok);
    BOOST_REQUIRE_EQUAL(httpResponse.second, "secure loopback response");
    BOOST_REQUIRE(tlsServer.responseSent());
    BOOST_REQUIRE_EQUAL(tlsServer.requestTarget(), "/secure");
    BOOST_REQUIRE_EQUAL(tlsServer.sniHostname(), "localhost");
}

BOOST_AUTO_TEST_CASE(trusted_https_rejects_hostname_mismatch) {
    LoopbackTlsServer tlsServer(
        LoopbackTlsServer::Exchange::ExpectRejectedHandshake);
    TrustedTestHttpClient httpClient(
        "127.0.0.1", tlsServer.port(), std::chrono::milliseconds(1000));

    const boost::system::error_code executionError =
        captureExecutionError(httpClient, "/hostname-mismatch");
    tlsServer.waitForCompletion();

    BOOST_REQUIRE(executionError);
    BOOST_REQUIRE(executionError != beast::error::timeout);
    BOOST_REQUIRE(tlsServer.handshakeRejected());
    BOOST_REQUIRE(tlsServer.sniHostname().empty());
}

BOOST_AUTO_TEST_CASE(default_https_rejects_untrusted_self_signed_certificate) {
    LoopbackTlsServer tlsServer(
        LoopbackTlsServer::Exchange::ExpectRejectedHandshake);
    HttpClientImpl httpClient(
        "localhost",
        tlsServer.port(),
        HttpClientImpl::Transport::Https,
        11,
        std::chrono::milliseconds(1000));

    const boost::system::error_code executionError =
        captureExecutionError(httpClient, "/untrusted");
    tlsServer.waitForCompletion();

    BOOST_REQUIRE(executionError);
    BOOST_REQUIRE(executionError != beast::error::timeout);
    BOOST_REQUIRE(tlsServer.handshakeRejected());
}

BOOST_AUTO_TEST_CASE(plain_http_read_stall_throws_beast_timeout) {
    PlainStallServer stallServer;
    HttpClientImpl httpClient(
        "127.0.0.1",
        stallServer.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(100));

    const boost::system::error_code executionError =
        captureExecutionError(httpClient, "/stall");
    stallServer.stopAndWait();

    BOOST_REQUIRE(stallServer.connectionAccepted());
    BOOST_REQUIRE(executionError == beast::error::timeout);
}

BOOST_AUTO_TEST_CASE(configurable_response_body_limit_is_enforced) {
    const std::string responseBody = "response exceeds eight bytes";
    PlainResponseServer rejectedResponseServer(responseBody);
    HttpClientImpl limitedHttpClient(
        "127.0.0.1",
        rejectedResponseServer.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000),
        8);

    const boost::system::error_code limitError =
        captureExecutionError(limitedHttpClient, "/limited");
    rejectedResponseServer.waitForCompletion();

    BOOST_REQUIRE(limitError == http::error::body_limit);

    PlainResponseServer acceptedResponseServer(responseBody);
    HttpClientImpl accommodatingHttpClient(
        "127.0.0.1",
        acceptedResponseServer.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000),
        responseBody.size());

    const std::pair<http::status, std::string> acceptedResponse =
        accommodatingHttpClient.execute("GET", "/accepted", "", {});
    acceptedResponseServer.waitForCompletion();

    BOOST_REQUIRE_EQUAL(acceptedResponse.first, http::status::ok);
    BOOST_REQUIRE_EQUAL(acceptedResponse.second, responseBody);
}

BOOST_AUTO_TEST_CASE(https_response_body_limit_is_enforced) {
    LoopbackTlsServer tlsServer(
        LoopbackTlsServer::Exchange::SendResponse,
        "TLS response exceeds eight bytes");
    TrustedTestHttpClient httpClient(
        "localhost",
        tlsServer.port(),
        std::chrono::milliseconds(1000),
        8);

    const boost::system::error_code executionError =
        captureExecutionError(httpClient, "/limited");
    tlsServer.waitForCompletion();

    BOOST_REQUIRE(executionError == http::error::body_limit);
    BOOST_REQUIRE(tlsServer.responseSent());
}

class ChunkedStreamingServer final {
public:
    ChunkedStreamingServer(const std::vector<std::string> &chunks,
                           std::chrono::milliseconds interChunkDelay =
                               std::chrono::milliseconds::zero(),
                           http::status responseStatus = http::status::ok)
        : acceptor_(serverIoContext_,
                    tcp::endpoint(asio::ip::address_v4::loopback(), 0)),
          chunks_(chunks),
          interChunkDelay_(interChunkDelay),
          responseStatus_(responseStatus),
          serverThread_(&ChunkedStreamingServer::serve, this) {}

    ~ChunkedStreamingServer() {
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
    }

    ChunkedStreamingServer(const ChunkedStreamingServer &) = delete;
    ChunkedStreamingServer &operator=(const ChunkedStreamingServer &) = delete;

    std::string port() const {
        return std::to_string(acceptor_.local_endpoint().port());
    }

    void waitForCompletion() {
        if (serverThread_.joinable()) {
            serverThread_.join();
        }
        if (serverException_) {
            std::rethrow_exception(serverException_);
        }
    }

private:
    void serve() {
        try {
            tcp::socket acceptedSocket =
                acceptLoopbackConnection(acceptor_, serverIoContext_);

            const std::string statusLine =
                "HTTP/1.1 " +
                std::to_string(static_cast<int>(responseStatus_)) + " " +
                std::string(http::obsolete_reason(responseStatus_)) + "\r\n"
                "Content-Type: text/event-stream\r\n"
                "Cache-Control: no-cache\r\n"
                "Connection: close\r\n"
                "\r\n";

            beast::error_code writeError;
            asio::write(acceptedSocket,
                        asio::buffer(statusLine.data(), statusLine.size()),
                        writeError);
            if (writeError) {
                throw boost::system::system_error(writeError,
                                                  "write response header");
            }

            for (const auto &chunk : chunks_) {
                if (interChunkDelay_ !=
                    std::chrono::milliseconds::zero()) {
                    std::this_thread::sleep_for(interChunkDelay_);
                }

                asio::write(acceptedSocket,
                            asio::buffer(chunk.data(), chunk.size()),
                            writeError);
                if (writeError) {
                    throw boost::system::system_error(
                        writeError, "write body chunk");
                }
            }

            beast::error_code shutdownError;
            acceptedSocket.shutdown(tcp::socket::shutdown_both,
                                    shutdownError);
        } catch (...) {
            serverException_ = std::current_exception();
        }
    }

    asio::io_context serverIoContext_;
    tcp::acceptor acceptor_;
    std::vector<std::string> chunks_;
    std::chrono::milliseconds interChunkDelay_;
    http::status responseStatus_;
    std::exception_ptr serverException_;
    std::thread serverThread_;
};

BOOST_AUTO_TEST_CASE(http_stream_frames_sse_events_across_chunks) {
    // Event boundaries cross TCP write chunks; framer must reassemble.
    const std::vector<std::string> wireChunks{
        "data: {\"n\":1}\n\n"
        "da",
        "ta: {\"n\":2}\n",
        "\n"
        ": heartbeat\n"
        "data: line-a\n"
        "data: line-b\n"
        "\n"};
    ChunkedStreamingServer server(wireChunks,
                                  std::chrono::milliseconds(50));
    HttpClientImpl httpClient(
        "127.0.0.1",
        server.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000));

    std::vector<std::string> receivedEvents;
    std::mutex eventMutex;
    const http::status status = httpClient.executeStream(
        "GET", "/stream", "", {},
        [&receivedEvents, &eventMutex](const std::string &eventData) {
            std::lock_guard<std::mutex> lock(eventMutex);
            receivedEvents.push_back(eventData);
        });

    server.waitForCompletion();

    BOOST_REQUIRE_EQUAL(status, http::status::ok);
    BOOST_REQUIRE_EQUAL(receivedEvents.size(), 3U);
    BOOST_REQUIRE_EQUAL(receivedEvents[0], "{\"n\":1}");
    BOOST_REQUIRE_EQUAL(receivedEvents[1], "{\"n\":2}");
    BOOST_REQUIRE_EQUAL(receivedEvents[2], "line-a\nline-b");
}

BOOST_AUTO_TEST_CASE(http_stream_strips_split_utf8_bom) {
    const std::vector<std::string> wireChunks{
        std::string(1, static_cast<char>(0xef)),
        std::string(1, static_cast<char>(0xbb)),
        std::string(1, static_cast<char>(0xbf)) + "data: first-event\n\n"};
    ChunkedStreamingServer server(wireChunks);
    HttpClientImpl httpClient(
        "127.0.0.1",
        server.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000));

    std::vector<std::string> receivedEvents;
    const http::status status = httpClient.executeStream(
        "GET", "/bom", "", {},
        [&receivedEvents](const std::string &eventData) {
            receivedEvents.push_back(eventData);
        });
    server.waitForCompletion();

    BOOST_REQUIRE_EQUAL(status, http::status::ok);
    BOOST_REQUIRE_EQUAL(receivedEvents.size(), 1U);
    BOOST_REQUIRE_EQUAL(receivedEvents.front(), "first-event");
}

BOOST_AUTO_TEST_CASE(http_stream_refills_buffer_body_storage) {
    const std::string eventPayload(70000, 'x');
    ChunkedStreamingServer server({"data: " + eventPayload + "\n\n"});
    HttpClientImpl httpClient(
        "127.0.0.1",
        server.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000));

    std::vector<std::string> receivedEvents;
    const http::status status = httpClient.executeStream(
        "GET", "/large-event", "", {},
        [&receivedEvents](const std::string &eventData) {
            receivedEvents.push_back(eventData);
        });
    server.waitForCompletion();

    BOOST_REQUIRE_EQUAL(status, http::status::ok);
    BOOST_REQUIRE_EQUAL(receivedEvents.size(), 1U);
    BOOST_REQUIRE_EQUAL(receivedEvents.front(), eventPayload);
}

BOOST_AUTO_TEST_CASE(http_stream_empty_body) {
    ChunkedStreamingServer server({});
    HttpClientImpl httpClient(
        "127.0.0.1",
        server.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000));

    std::vector<std::string> receivedEvents;
    const http::status status = httpClient.executeStream(
        "GET", "/empty", "", {},
        [&receivedEvents](const std::string &eventData) {
            receivedEvents.push_back(eventData);
        });

    server.waitForCompletion();

    BOOST_REQUIRE_EQUAL(status, http::status::ok);
    BOOST_REQUIRE(receivedEvents.empty());
}

BOOST_AUTO_TEST_CASE(http_stream_single_sse_event) {
    const std::string wire = "data: {\"ok\":true}\n\n";
    ChunkedStreamingServer server({wire});
    HttpClientImpl httpClient(
        "127.0.0.1",
        server.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000));

    std::vector<std::string> receivedEvents;
    const http::status status = httpClient.executeStream(
        "GET", "/single", "", {},
        [&receivedEvents](const std::string &eventData) {
            receivedEvents.push_back(eventData);
        });

    server.waitForCompletion();

    BOOST_REQUIRE_EQUAL(status, http::status::ok);
    BOOST_REQUIRE_EQUAL(receivedEvents.size(), 1U);
    BOOST_REQUIRE_EQUAL(receivedEvents[0], "{\"ok\":true}");
}

BOOST_AUTO_TEST_CASE(http_stream_discards_incomplete_event_at_eof) {
    ChunkedStreamingServer server({"data: incomplete-without-blank-line"});
    HttpClientImpl httpClient(
        "127.0.0.1",
        server.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000));

    std::vector<std::string> receivedEvents;
    const http::status status = httpClient.executeStream(
        "GET", "/incomplete", "", {},
        [&receivedEvents](const std::string &eventData) {
            receivedEvents.push_back(eventData);
        });

    server.waitForCompletion();

    BOOST_REQUIRE_EQUAL(status, http::status::ok);
    BOOST_REQUIRE(receivedEvents.empty());
}

BOOST_AUTO_TEST_CASE(http_stream_body_limit_is_enforced) {
    ChunkedStreamingServer server({"data: exceeds eight bytes\n\n"});
    HttpClientImpl limitedHttpClient(
        "127.0.0.1",
        server.port(),
        HttpClientImpl::Transport::Http,
        11,
        std::chrono::milliseconds(1000),
        8);

    bool exceptionThrown = false;
    try {
        limitedHttpClient.executeStream(
            "GET", "/limited", "", {},
            [](const std::string &) {});
    } catch (const boost::system::system_error &) {
        exceptionThrown = true;
    }
    server.waitForCompletion();

    BOOST_REQUIRE(exceptionThrown);
}

BOOST_AUTO_TEST_SUITE_END()
