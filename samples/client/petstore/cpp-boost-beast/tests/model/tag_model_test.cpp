#define BOOST_TEST_INCLUDED
#include <list>
#include <boost/property_tree/ptree.hpp>
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "../ApprovalTests.hpp"

#include "model/Tag.h"

using namespace ApprovalTests;
using namespace org::openapitools::client::model;

BOOST_AUTO_TEST_SUITE(TagModelTest)

BOOST_AUTO_TEST_CASE(toJsonString) {
  Tag tag;
  tag.setId(1);
  tag.setName("MyTag");
  Approvals::verify(tag.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(fromJsonString) {
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

BOOST_AUTO_TEST_CASE(test_createJsonStringFromTagVector) {
  auto tag0 = std::make_shared<Tag>();
  auto tag1 = std::make_shared<Tag>();
  auto tag2 = std::make_shared<Tag>();

  tag0->setId(23);
  tag0->setName("HelloTag");
  tag1->setId(42);
  tag1->setName("MyTag");
  tag2->setId(13);
  tag2->setName("OtherTag");

  auto vec = std::vector<std::shared_ptr<Tag>>();
  
  vec.emplace_back(tag0);
  vec.emplace_back(tag1);
  vec.emplace_back(tag2);

  const auto json = createJsonStringFromTagVector(vec);

  Approvals::verify(json);
}

BOOST_AUTO_TEST_CASE(ToAndFromPropertyTree) {
  Tag tag;
  tag.setId(42);
  tag.setName("AnotherTag");

  const auto pt = tag.toPropertyTree();

  const auto newTag = Tag(pt);

  BOOST_TEST(newTag.getId() == 42);
  BOOST_TEST(newTag.getName() == "AnotherTag");
}

BOOST_AUTO_TEST_SUITE_END()
