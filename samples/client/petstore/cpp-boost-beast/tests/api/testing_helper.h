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

#include <vector>
#include <tuple>

using namespace ApprovalTests;
using namespace org::openapitools::client::api;


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
            const std::map<std::string, std::string>) override {

        switch (m_exceptionType) {
            case ExceptionType::STD_EXCEPTION:
                throw std::logic_error("C++ exception thrown");
            case ExceptionType::INT:
                throw 23;
        }

        return {boost::beast::http::status{500}, "this should not happen"};
    }

private:
    ExceptionType m_exceptionType;
};
