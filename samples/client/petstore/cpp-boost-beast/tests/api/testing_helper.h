#define BOOST_TEST_INCLUDED
#include <list>
#include "../ApprovalTests.hpp"
#include <boost/property_tree/ptree.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>
#include <initializer_list>

#include "api/HttpClientImpl.h"
#include "api/PetApi.h"
#include "model/Pet.h"

#include <functional>
#include <map>
#include <string>
#include <utility>
#include <vector>
#include <tuple>

using namespace ApprovalTests;
using namespace org::openapitools::client::api;


#define REQUIRE_THROW(fn, ex, lambda) do { \
        bool exceptionThrown = false;      \
        try { fn; }                        \
        catch(const ex& e) {               \
            exceptionThrown = true;        \
            lambda(e);                     \
         }                                 \
         BOOST_REQUIRE(exceptionThrown);   \
    } while (0)


enum class ExceptionType {
    STD_EXCEPTION,
    INT
};


inline std::ostream&
operator<<( std::ostream& ostr, ExceptionType const& et )
{
    switch (et) {
        case ExceptionType::STD_EXCEPTION:
            ostr << "STD_EXCEPTION";
            break;
        case ExceptionType::INT:
            ostr << "INT";
    }
    return ostr;
}

class ThrowingClient : public HttpClient {
public:

    void setExceptionType(ExceptionType exceptionType) {
        m_exceptionType = exceptionType;
    }

    std::pair<boost::beast::http::status, std::string>
    execute(const std::string&,
            const std::string&,
            const std::string&,
            const std::map<std::string, std::string>&) override {

        switch (m_exceptionType) {
            case ExceptionType::STD_EXCEPTION:
                throw std::logic_error("C++ exception thrown");
            case ExceptionType::INT:
                throw 23;
        }

        return {boost::beast::http::status{500}, "this should not happen"};
    }

    boost::beast::http::status
    executeStream(const std::string&,
                  const std::string&,
                  const std::string&,
                  const std::map<std::string, std::string>&,
                  std::function<void(const std::string &)>) override {
        throw std::logic_error("executeStream not implemented");
    }

private:
    ExceptionType m_exceptionType;
};

class RecordingClient : public HttpClient {
public:
    RecordingClient(boost::beast::http::status responseStatus,
                    std::string responseBody)
        : m_responseStatus(responseStatus),
          m_responseBody(std::move(responseBody)) {
    }

    std::pair<boost::beast::http::status, std::string>
    execute(const std::string& verb,
            const std::string& target,
            const std::string& body,
            const std::map<std::string, std::string>& headers) override {
        m_verb = verb;
        m_target = target;
        m_body = body;
        m_headers = headers;
        return {m_responseStatus, m_responseBody};
    }

    boost::beast::http::status
    executeStream(const std::string& verb,
                  const std::string& target,
                  const std::string& body,
                  const std::map<std::string, std::string>& headers,
                  std::function<void(const std::string &)> onEvent) override {
        execute(verb, target, body, headers);
        onEvent(m_responseBody);
        return m_responseStatus;
    }

    const std::string& target() const {
        return m_target;
    }

    const std::string& body() const {
        return m_body;
    }

    const std::map<std::string, std::string>& headers() const {
        return m_headers;
    }

private:
    boost::beast::http::status m_responseStatus;
    std::string m_responseBody;
    std::string m_verb;
    std::string m_target;
    std::string m_body;
    std::map<std::string, std::string> m_headers;
};
