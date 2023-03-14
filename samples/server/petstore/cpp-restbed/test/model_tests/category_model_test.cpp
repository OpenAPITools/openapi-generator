#define BOOST_TEST_INCLUDED
#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>


#include "model/Category.h"

using namespace org::openapitools::server::model;


BOOST_AUTO_TEST_SUITE(CategoryModelTest)

BOOST_AUTO_TEST_CASE(toJsonString)
{
  Category category;
  category.setId(1);
  category.setName("category1");

  const auto json = category.toJsonString(false);

  Category newCategory;
  newCategory.fromJsonString(json);

  BOOST_TEST(newCategory.getId() == 1);
  BOOST_TEST(newCategory.getName() == "category1");
}

BOOST_AUTO_TEST_CASE(fromJsonString)
{
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

BOOST_AUTO_TEST_CASE(fromJsonArrayString)
{
  const std::string json = R"JSON(
[{
    "id": "1",
    "name": "categoryA"
},
{
    "id": "2",
    "name": "categoryB"
},
{
    "id": "3",
    "name": "categoryC"
}]
)JSON";

  const auto categoryVec = createCategoryVectorFromJsonString(json);

  BOOST_TEST(categoryVec.size() == 3);
  BOOST_TEST(categoryVec[0].getId() == 1);
  BOOST_TEST(categoryVec[0].getName() == "categoryA");
  BOOST_TEST(categoryVec[1].getId() == 2);
  BOOST_TEST(categoryVec[1].getName() == "categoryB");
  BOOST_TEST(categoryVec[2].getId() == 3);
  BOOST_TEST(categoryVec[2].getName() == "categoryC");

}

BOOST_AUTO_TEST_CASE(fromAndToPropertyTree)
{
  Category category;
  category.setId(23);
  category.setName("categoryX");

  const auto pt = category.toPropertyTree();

  const auto newCategory = Category(pt);
  BOOST_TEST(newCategory.getId() == 23);
  BOOST_TEST(newCategory.getName() == "categoryX");
}

BOOST_AUTO_TEST_SUITE_END()
