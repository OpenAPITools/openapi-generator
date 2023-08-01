
#define BOOST_TEST_INCLUDED

#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>


#include "model/AdditionalPropertiesClass.h"
#include "test_helper.h"

using namespace org::openapitools::server::model;

namespace {
    const std::string jsonForTest = R"({
  "map_property" : {
    "additionalProp1" : "string",
    "additionalProp2" : "string",
    "additionalProp3" : "string"
  },
  "map_of_map_property" : {
    "additionalProp1" : {
      "additionalProp1" : "string",
      "additionalProp2" : "string",
      "additionalProp3" : "string"
    },
    "additionalProp2" : {
      "additionalProp1" : "string",
      "additionalProp2" : "string",
      "additionalProp3" : "string"
    },
    "additionalProp3" : {
      "additionalProp1" : "string",
      "additionalProp2" : "string",
      "additionalProp3" : "string"
    }
  }
}
        )";
}

BOOST_AUTO_TEST_SUITE(AdditionalPropertiesClassModelTest)

BOOST_AUTO_TEST_CASE(toJsonString)
{
    auto obj = AdditionalPropertiesClass();
    std::map<std::string, std::string> mapProperty;
    mapProperty.insert(std::make_pair("foo", "bar"));
    obj.setMapProperty(mapProperty);

    const auto jsonString = obj.toJsonString();

    auto newObj = AdditionalPropertiesClass();
    newObj.fromJsonString(jsonString);

    auto map = newObj.getMapProperty();
    BOOST_TEST(map.size() == 1);

    auto prop = map["foo"];
    BOOST_TEST(prop == "bar");
}

BOOST_AUTO_TEST_CASE(fromJsonAndBack)
{
    auto obj = AdditionalPropertiesClass();
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
    const auto vec = createAdditionalPropertiesClassVectorFromJsonString(jsonString);
    BOOST_TEST(vec.size() == 2);
}

BOOST_AUTO_TEST_SUITE_END()