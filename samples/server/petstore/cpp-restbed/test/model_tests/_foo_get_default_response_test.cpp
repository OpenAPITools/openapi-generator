#define BOOST_TEST_INCLUDED
#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>


#include "model/_foo_get_default_response.h"

using namespace org::openapitools::server::model;


BOOST_AUTO_TEST_SUITE(GetDefaultResponseModelTest)

    BOOST_AUTO_TEST_CASE(toJsonString)
    {
        auto defaultResponse = _foo_get_default_response();
        auto foo = Foo();
        foo.setBar("bar1");
        defaultResponse.setString(foo);

        const auto jsonString = defaultResponse.toJsonString();

        auto newDefaultResponse = _foo_get_default_response();
        newDefaultResponse.fromJsonString(jsonString);

        BOOST_TEST(newDefaultResponse.getString().getBar() == "bar1");
    }

    BOOST_AUTO_TEST_CASE(vectorFromJsonString)
    {
        const auto jsonString = R"([{"string":{"bar":"bar1"}}, {"string":{"bar":"bar2"}}])";
        const auto vec = create_foo_get_default_responseVectorFromJsonString(jsonString);
        BOOST_TEST(vec.size() == 2);
        BOOST_TEST(vec[0].getString().getBar() == "bar1");
        BOOST_TEST(vec[1].getString().getBar() == "bar2");
    }

BOOST_AUTO_TEST_SUITE_END()