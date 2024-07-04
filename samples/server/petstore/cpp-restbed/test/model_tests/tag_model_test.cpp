#define BOOST_TEST_INCLUDED
#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>

#include "model/Tag.h"

using namespace org::openapitools::server::model;


BOOST_AUTO_TEST_SUITE(TagModelTest)

BOOST_AUTO_TEST_CASE(toJsonString)
{
  Tag tag;
  tag.setId(1);
  tag.setName("MyTag");
  const auto json = tag.toJsonString(true);

  Tag newTag;
  newTag.fromJsonString(json);
  BOOST_TEST(newTag.getId() == 1);
  BOOST_TEST(newTag.getName() == "MyTag");

}

BOOST_AUTO_TEST_CASE(fromJsonString)
{
  const std::string json = R"JSON(
{
    "id": 23,
    "name": "HelloTag"
})JSON";

  Tag tag;
  tag.fromJsonString(json);

  BOOST_TEST(tag.getId() == 23);
  BOOST_TEST(tag.getName() == "HelloTag");
}


BOOST_AUTO_TEST_CASE(fromJsonArrayString)
{
  const std::string json = R"JSON(
[{
    "id": 23,
    "name": "HelloTag"
},{
    "id": 42,
    "name": "MyTag"
},{
    "id": 13,
    "name": "OtherTag"
}])JSON";

  const auto tagVector = createTagVectorFromJsonString(json);

  BOOST_TEST(tagVector.size() == 3);
  BOOST_TEST(tagVector[0].getId() == 23);
  BOOST_TEST(tagVector[0].getName() == "HelloTag");
  BOOST_TEST(tagVector[1].getId() == 42);
  BOOST_TEST(tagVector[1].getName() == "MyTag");
  BOOST_TEST(tagVector[2].getId() == 13);
  BOOST_TEST(tagVector[2].getName() == "OtherTag");
}


BOOST_AUTO_TEST_CASE(ToAndFromPropertyTree)
{
  Tag tag;
  tag.setId(42);
  tag.setName("AnotherTag");

  const auto pt = tag.toPropertyTree();

  const auto newTag = Tag(pt);

  BOOST_TEST(newTag.getId() == 42);
  BOOST_TEST(newTag.getName() == "AnotherTag");
}

BOOST_AUTO_TEST_SUITE_END()
