#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <iostream>
#include <string>
#include <utility>

#include "beast_client.h"

namespace beast = boost::beast;     // from <boost/beast.hpp>
namespace http = beast::http;       // from <boost/beast/http.hpp>
namespace net = boost::asio;        // from <boost/asio.hpp>
using tcp = net::ip::tcp;           // from <boost/asio/ip/tcp.hpp>


std::pair<int, std::string> requestData(const http::verb verb,
                        const std::string& target,
                        const std::string& data)
{
  std::string out;
  int status = 0;

  auto const host = "localhost";
  auto const port = "1234";
  int version = 11;

  net::io_context ioc;

  tcp::resolver resolver(ioc);
  beast::tcp_stream stream(ioc);

  auto const results = resolver.resolve(host, port);

  stream.connect(results);

  http::request<http::string_body> req{verb, target, version};
  req.set(http::field::host, host);
  req.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);
  req.set(http::field::body, data);

  http::write(stream, req);

  beast::flat_buffer buffer;
  http::response<http::dynamic_body> res;
  http::read(stream, buffer, res);
  out = boost::beast::buffers_to_string(res.body().data());
  status = static_cast<int>(res.result());

  beast::error_code ec;
  stream.socket().shutdown(tcp::socket::shutdown_both, ec);

  if(ec && ec != beast::errc::not_connected)
    throw beast::system_error{ec};

  return std::make_pair<int, std::string>(std::move(status), std::move(out));
}