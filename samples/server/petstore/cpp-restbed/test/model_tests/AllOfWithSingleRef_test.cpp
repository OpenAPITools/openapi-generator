
#define BOOST_TEST_INCLUDED

#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>
#include "test_helper.h"


#include "model/AllOfWithSingleRef.h"

using namespace org::openapitools::server::model;

namespace {
    const std::string jsonForTest = R"(
            {
              "username" : "string",
              "SingleRefType" : { }
            }
        )";
}

BOOST_AUTO_TEST_SUITE(AllOfWithSingleRefModelTest)

BOOST_AUTO_TEST_CASE(fromJsonAndBack)
{
    auto obj = AllOfWithSingleRef();
    obj.fromJsonString(jsonForTest);

    const auto newJson = obj.toJsonString();

    TEST_COMPARE_JSON(jsonForTest, newJson);
}

BOOST_AUTO_TEST_CASE(vectorFromJsonString)
{
    const auto jsonString = "[" +
                            jsonForTest +
                            "," +
                            jsonForTest +
                            "]";
    const auto vec = createAllOfWithSingleRefVectorFromJsonString(jsonString);
    BOOST_TEST(vec.size() == 2);
}

BOOST_AUTO_TEST_SUITE_END()