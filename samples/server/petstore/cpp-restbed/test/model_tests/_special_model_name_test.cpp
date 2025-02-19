
#define BOOST_TEST_INCLUDED

#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>


#include "model/_special_model_name_.h"

using namespace org::openapitools::server::model;

namespace {
_special_model_name_ create_special_model_name_forTest() {
    auto obj = _special_model_name_();
    obj.setSpecialPropertyName(12);
    return obj;
}
}

BOOST_AUTO_TEST_SUITE(special_model_name_ModelTest)

BOOST_AUTO_TEST_CASE(toJsonString)
{
    const auto obj = create_special_model_name_forTest();
    const auto jsonString = obj.toJsonString();

    auto newObj = _special_model_name_();
    newObj.fromJsonString(jsonString);

    BOOST_TEST(newObj.getSpecialPropertyName() == 12);
}

BOOST_AUTO_TEST_CASE(vectorFromJsonString)
{
    const auto obj = create_special_model_name_forTest();

    const auto jsonString = "[" +
            obj.toJsonString() +
            "," +
            obj.toJsonString() +
            "]";
    const auto vec = create_special_model_name_VectorFromJsonString(jsonString);
    BOOST_TEST(vec.size() == 2);
}

BOOST_AUTO_TEST_SUITE_END()