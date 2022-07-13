
#define BOOST_TEST_INCLUDED

#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>


#include "model/AdditionalPropertiesClass.h"

using namespace org::openapitools::server::model;


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

    BOOST_AUTO_TEST_CASE(vectorFromJsonString)
    {
        /*const auto jsonString = R"([{}])";
        const auto vec = create_AdditionalPropertiesClass(jsonString);
        BOOST_TEST(vec.size() == 99);
        BOOST_TEST(vec[0].get ==);*/
    }

BOOST_AUTO_TEST_SUITE_END()