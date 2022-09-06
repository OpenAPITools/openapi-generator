#pragma once

#include <boost/beast/http.hpp>

std::pair<int, std::string> requestData(const boost::beast::http::verb verb,
                        const std::string& target,
                        const std::string& data = "");
