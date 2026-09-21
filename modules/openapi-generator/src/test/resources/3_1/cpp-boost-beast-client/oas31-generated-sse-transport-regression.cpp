#include "api/HttpClientImpl.cpp"

#include <boost/asio/write.hpp>
#include <boost/beast/http.hpp>

#include <chrono>
#include <exception>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

namespace api = org::openapitools::client::api;

namespace {

void expect(bool condition, const char* message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

template <typename Callable>
void expectLengthError(Callable&& callable, const char* message) {
    bool rejected = false;
    try {
        std::forward<Callable>(callable)();
    } catch (const std::length_error&) {
        rejected = true;
    }
    expect(rejected, message);
}

void testFraming() {
    api::SseStreamOptions options;
    options.maxLineBytes = 64;
    options.maxEventBytes = 256;
    std::vector<api::SseEvent> events;
    SseEventFramer framer(
        [&events](const api::SseEvent& event) {
            events.push_back(event);
            return true;
        },
        options);

    const char byteOrderMark[] = {
        static_cast<char>(0xef), static_cast<char>(0xbb),
        static_cast<char>(0xbf)};
    expect(framer.feed(std::string_view(byteOrderMark, 1)),
           "split BOM byte cancelled the stream");
    expect(framer.feed(std::string_view(byteOrderMark + 1, 1)),
           "split BOM byte cancelled the stream");
    expect(framer.feed(std::string_view(byteOrderMark + 2, 1)),
           "split BOM byte cancelled the stream");
    expect(framer.feed(": comment\r"), "split comment line cancelled the stream");
    expect(framer.feed("\nevent: delta\r\nid: event-1\r\nretry: 250\r\n"),
           "event metadata cancelled the stream");
    expect(framer.feed("data: first\r\ndata: second\r\n\r"),
           "split data event cancelled the stream");
    expect(framer.feed("\n"), "event terminator cancelled the stream");

    std::string invalidId = "id: bad";
    invalidId.push_back('\0');
    invalidId += "id\r\n";
    expect(framer.feed(invalidId), "NUL-containing id cancelled the stream");
    expect(framer.feed("retry: 25x\r\ndata\r\n\r\n"),
           "empty data event cancelled the stream");
    expect(framer.feed("data: unfinished"),
           "unterminated data cancelled the stream");
    framer.finish();

    expect(events.size() == 2U,
           "SSE framing dispatched the wrong number of complete events");
    expect(events[0].event == "delta" && events[0].data == "first\nsecond"
               && events[0].id == "event-1"
               && events[0].retryMilliseconds == 250U,
           "SSE framing lost event, data, id, or retry metadata");
    expect(events[1].event == "message" && events[1].data.empty()
               && events[1].id == "event-1"
               && !events[1].retryMilliseconds.has_value(),
           "SSE framing mishandled empty data, persistent id, or invalid retry");

    api::SseStreamOptions eofOptions;
    eofOptions.dispatchUnterminatedEventAtEof = true;
    std::vector<api::SseEvent> eofEvents;
    SseEventFramer eofFramer(
        [&eofEvents](const api::SseEvent& event) {
            eofEvents.push_back(event);
            return true;
        },
        eofOptions);
    expect(eofFramer.feed("event: terminal\ndata: final"),
           "unterminated compatibility event cancelled the stream");
    eofFramer.finish();
    expect(eofEvents.size() == 1U && eofEvents[0].event == "terminal"
               && eofEvents[0].data == "final",
           "EOF compatibility did not dispatch the final unterminated event");

    api::SseStreamOptions exactLineOptions;
    exactLineOptions.maxLineBytes = 7;
    exactLineOptions.maxEventBytes = 64;
    std::vector<api::SseEvent> exactLineEvents;
    SseEventFramer exactLineFramer(
        [&exactLineEvents](const api::SseEvent& event) {
            exactLineEvents.push_back(event);
            return true;
        },
        exactLineOptions);
    expect(exactLineFramer.feed("data: x\r"),
           "a split CR made an exact-limit line fail");
    expect(exactLineFramer.feed("\n\r\n"),
           "a split CRLF terminator cancelled the stream");
    expect(exactLineEvents.size() == 1U && exactLineEvents[0].data == "x",
           "split CRLF framing lost an exact-limit event");

    api::SseStreamOptions lineLimitOptions;
    lineLimitOptions.maxLineBytes = 3;
    lineLimitOptions.maxEventBytes = 64;
    SseEventFramer lineLimitFramer(
        [](const api::SseEvent&) { return true; }, lineLimitOptions);
    expectLengthError(
        [&lineLimitFramer]() { (void)lineLimitFramer.feed("data"); },
        "SSE line limit did not reject an oversized unterminated line");

    api::SseStreamOptions eventLimitOptions;
    eventLimitOptions.maxLineBytes = 64;
    eventLimitOptions.maxEventBytes = 7;
    SseEventFramer eventLimitFramer(
        [](const api::SseEvent&) { return true; }, eventLimitOptions);
    expectLengthError(
        [&eventLimitFramer]() { (void)eventLimitFramer.feed("data: x\n\n"); },
        "SSE event limit did not include the default event type");

    api::SseStreamOptions cancellationOptions;
    SseEventFramer cancellationFramer(
        [](const api::SseEvent&) { return false; }, cancellationOptions);
    expect(!cancellationFramer.feed("data: stop\n\n")
               && cancellationFramer.cancelled(),
           "SSE callback cancellation did not stop the framer");
}

void writeAll(boost::asio::ip::tcp::socket& socket,
              const std::string_view data) {
    boost::beast::error_code error;
    boost::asio::write(socket, boost::asio::buffer(data.data(), data.size()), error);
    if (error) {
        throw boost::system::system_error(error);
    }
}

void writeChunk(boost::asio::ip::tcp::socket& socket,
                const std::string_view data) {
    std::ostringstream prefix;
    prefix << std::hex << data.size() << "\r\n";
    writeAll(socket, prefix.str());
    writeAll(socket, data);
    writeAll(socket, "\r\n");
}

void testLoopbackTransport() {
    boost::asio::io_context serverIo;
    boost::asio::ip::tcp::acceptor acceptor(
        serverIo,
        {boost::asio::ip::address_v4::loopback(), 0});
    const std::string port = std::to_string(acceptor.local_endpoint().port());
    std::exception_ptr serverFailure;

    std::thread server([&]() {
        try {
            boost::asio::ip::tcp::socket socket(serverIo);
            acceptor.accept(socket);
            boost::beast::flat_buffer requestBuffer;
            boost::beast::http::request<boost::beast::http::string_body> request;
            boost::beast::http::read(socket, requestBuffer, request);
            expect(request.method() == boost::beast::http::verb::get
                       && request.target() == "/events",
                   "loopback server received the wrong request");

            writeAll(socket,
                     "HTTP/1.1 200 OK\r\n"
                     "Content-Type: TEXT/EVENT-STREAM ; charset=utf-8\r\n"
                     "Transfer-Encoding: chunked\r\n"
                     "Connection: close\r\n\r\n");
            const char byteOrderMark[] = {
                static_cast<char>(0xef), static_cast<char>(0xbb),
                static_cast<char>(0xbf)};
            writeChunk(socket, std::string_view(byteOrderMark, 1));
            writeChunk(socket, std::string_view(byteOrderMark + 1, 1));
            writeChunk(socket, std::string_view(byteOrderMark + 2, 1));
            writeChunk(socket, "event: delta\r");
            writeChunk(socket, "\nid: wire-1\nretry: 9\r\ndata: one\r\n");
            writeChunk(socket, "data: two\r\n\r");
            writeChunk(socket, "\ndata: unfinished");
            writeAll(socket, "0\r\n\r\n");
            boost::beast::error_code shutdownError;
            socket.shutdown(boost::asio::ip::tcp::socket::shutdown_send,
                            shutdownError);
        } catch (...) {
            serverFailure = std::current_exception();
        }
    });

    std::exception_ptr clientFailure;
    std::vector<api::SseEvent> events;
    api::HttpResponseData response;
    try {
        api::HttpClientImpl client(
            "127.0.0.1", port, api::HttpClientImpl::Transport::Http, 11,
            std::chrono::seconds(2), 1024);
        api::SseStreamOptions options;
        options.dispatchUnterminatedEventAtEof = true;
        response = client.executeStream(
            "GET", "/events", "", {},
            [&events](const api::SseEvent& event) {
                events.push_back(event);
                return true;
            }, options);
    } catch (...) {
        clientFailure = std::current_exception();
    }

    server.join();
    if (serverFailure) std::rethrow_exception(serverFailure);
    if (clientFailure) std::rethrow_exception(clientFailure);

    expect(response.status == boost::beast::http::status::ok
               && response.isEventStream && response.body.empty()
               && !response.streamCancelled,
           "streaming transport lost successful response metadata");
    expect(events.size() == 2U && events[0].event == "delta"
               && events[0].data == "one\ntwo" && events[0].id == "wire-1"
               && events[0].retryMilliseconds == 9U
               && events[1].event == "message" && events[1].data == "unfinished"
               && events[1].id == "wire-1"
               && !events[1].retryMilliseconds.has_value(),
           "streaming transport did not apply EOF dispatch compatibility");
}

} // namespace

int main() {
    try {
        testFraming();
        testLoopbackTransport();
        std::cout << "oas31 SSE transport regressions passed\n";
    } catch (const std::exception& exception) {
        std::cerr << exception.what() << '\n';
        return 1;
    }
    return 0;
}
