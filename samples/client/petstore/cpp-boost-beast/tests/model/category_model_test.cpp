#define BOOST_TEST_INCLUDED
#include <list>
#include <boost/property_tree/ptree.hpp>
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "../ApprovalTests.hpp"

#include "model/Category.h"

using namespace ApprovalTests;
using namespace org::openapitools::client::model;

BOOST_AUTO_TEST_SUITE(CategoryModelTest)

BOOST_AUTO_TEST_CASE(toJsonString) {
  Category category;
  category.setId(1);
  category.setName("category1");

  Approvals::verify(category.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(fromJsonString) {
  const std::string json = R"JSON(
{
    "id": "23",
    "name": "categoryB"
}
)JSON";

  Category category;
  category.fromJsonString(json);

  BOOST_TEST(category.getId() == 23);
  BOOST_TEST(category.getName() == "categoryB");
}

BOOST_AUTO_TEST_CASE(test_createJsonStringFromCategoryVector) {
  auto category0 = std::make_shared<Category>();
  auto category1 = std::make_shared<Category>();
  auto category2 = std::make_shared<Category>();

  category0->setId(1);
  category0->setName("categoryA");
  category1->setId(2);
  category1->setName("categoryB");
  category2->setId(3);
  category2->setName("categoryC");

  auto vec = std::vector<std::shared_ptr<Category>>();
  vec.emplace_back(category0);
  vec.emplace_back(category1);
  vec.emplace_back(category2);

  const auto json = createJsonStringFromModelVector(vec);

  Approvals::verify(json);
}

BOOST_AUTO_TEST_CASE(fromAndToPropertyTree) {
  Category category;
  category.setId(23);
  category.setName("categoryX");

  const auto pt = category.toPropertyTree();

  const auto newCategory = Category(pt);
  BOOST_TEST(newCategory.getId() == 23);
  BOOST_TEST(newCategory.getName() == "categoryX");
}

BOOST_AUTO_TEST_SUITE_END()
