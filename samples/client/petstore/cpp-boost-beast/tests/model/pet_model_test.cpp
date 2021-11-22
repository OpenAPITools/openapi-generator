#define BOOST_TEST_INCLUDED
#include <list>
#include "../ApprovalTests.hpp"
#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>
#include <initializer_list>
#include <sstream>

#include "model/Category.h"
#include "model/Pet.h"
#include "model/Tag.h"

using namespace ApprovalTests;
using namespace org::openapitools::client::model;

template <class T>
boost::property_tree::ptree
createPropertyTreeSubArray(const std::initializer_list<T> &data) {
  boost::property_tree::ptree tmp_node;
  for (const auto &tag : data) {
    boost::property_tree::ptree tagEntry;
    tagEntry.put("", tag);
    tmp_node.push_back(std::make_pair("", tagEntry));
  }

  return tmp_node;
}

BOOST_AUTO_TEST_SUITE(PetModelTest)

BOOST_AUTO_TEST_CASE(toJsonString) {
  Pet pet;
  pet.setId(1);
  pet.setName("MyName");
  Approvals::verify(pet.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(toJsonStringWithCategory) {
  Pet pet;
  pet.setId(1);
  pet.setName("MyName");
  auto category = std::make_shared<Category>();
  pet.setCategory(category);

  Approvals::verify(pet.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(toJsonStringWithTags) {
  Pet pet;
  pet.setId(1);
  pet.setName("MyName");

  std::vector<std::shared_ptr<Tag>> tags;
  auto tag1 = std::make_shared<Tag>();
  tag1->setName("tag1");
  tag1->setId(1);
  tags.emplace_back(tag1);
  auto tag2 = std::make_shared<Tag>();
  tag2->setName("tag2");
  tag2->setId(2);
  tags.emplace_back(tag2);
  pet.setTags(tags);

  Approvals::verify(pet.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(fromJsonString) {
  Pet pet;
  std::string json = R"JSON(
{
    "id": "23",
    "name": "ThePet",
    "status": "available"
})JSON";

  pet.fromJsonString(json);

  BOOST_TEST(pet.getId() == 23);
  BOOST_TEST(pet.getName() == "ThePet");
  BOOST_TEST(pet.getStatus() == "available");
}

BOOST_AUTO_TEST_CASE(test_createJsonStringFromPetVector) {
  auto pet0 = std::make_shared<Pet>();
  auto pet1 = std::make_shared<Pet>();

  pet0->setId(23);
  pet0->setName("ThePet");
  pet0->setStatus("available");

  pet1->setId(42);
  pet1->setName("TheOtherPet");
  pet1->setStatus("available");

  std::vector<std::shared_ptr<Pet>> vector;
  vector.emplace_back(pet0);
  vector.emplace_back(pet1);

  const auto jsonVector  = createJsonStringFromPetVector(vector);

  Approvals::verify(jsonVector);
}

BOOST_AUTO_TEST_CASE(fromJsonAndPropertyTree) {
  std::stringstream json_stream;
  json_stream << R"JSON(
{
    "id": "23",
    "name": "ThePet",
    "status": "available"
})JSON";

  boost::property_tree::ptree pt;
  boost::property_tree::read_json(json_stream, pt);

  Pet pet{pt};

  BOOST_TEST(pet.getId() == 23);
  BOOST_TEST(pet.getName() == "ThePet");
  BOOST_TEST(pet.getStatus() == "available");
}

BOOST_AUTO_TEST_CASE(fromPropertyTree) {

  boost::property_tree::ptree pt;

  pt.add("id", 11);
  pt.add("status", "available");
  pt.add("name", "Fluffy");

  boost::property_tree::ptree tags;
  boost::property_tree::ptree tagPt1;
  tagPt1.add("name", "tag1");
  tagPt1.add("id", 1);
  tags.push_back(std::make_pair("", tagPt1));
  boost::property_tree::ptree tagPt2;
  tagPt2.add("name", "tag2");
  tagPt2.add("id", 2);
  tags.push_back(std::make_pair("", tagPt2));
  pt.add_child("tags", tags);

  pt.add_child("photoUrls",
               createPropertyTreeSubArray(
                   {"www.example.com/photo1", "www.example.com/photo2"}));

  pt.add("category.name", "Category1");
  pt.add("category.id", 0);

  Pet pet;
  pet.fromPropertyTree(pt);

  std::stringstream stringstream;
  write_json(stringstream, pt, true);

  // Approvals::verify(stringstream.str());
  Approvals::verify(pet.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(toPropertyTree) {
  Pet pet;

  std::vector<std::shared_ptr<Tag>> tags;
  auto tag = std::make_shared<Tag>();
  tag->setName("Tag1");
  tags.emplace_back(tag);
  pet.setTags(tags);

  std::vector<std::string> photoUrls;
  photoUrls.emplace_back("www.example.com/photo1");
  photoUrls.emplace_back("www.example.com/photo2");
  pet.setPhotoUrls(photoUrls);

  auto category = std::make_shared<Category>();
  category->setName("Category1");
  pet.setCategory(category);

  auto pt = pet.toPropertyTree();

  BOOST_TEST(pt.get<int64_t>("id") == 0);
  BOOST_TEST(pt.get<std::string>("name") == "");
  BOOST_TEST(pt.get<std::string>("status") == "");

  auto tagsFromPt = pt.get_child("tags");
  BOOST_TEST(tagsFromPt.size() == 1);

  auto photoUrlsFromPt = pt.get_child("photoUrls");
  BOOST_TEST(photoUrlsFromPt.size() == 2);

  BOOST_TEST(pet.getCategory()->getName() == "Category1");
  auto categoryFromPt = pt.get_child("category");
  BOOST_TEST(categoryFromPt.get<std::string>("name") == "Category1");
}

BOOST_AUTO_TEST_CASE(photoUrls) {
  Pet pet;
  std::vector<std::string> photoUrls{"url1", "url2"};
  pet.setPhotoUrls(photoUrls);

  BOOST_TEST(pet.getPhotoUrls().size() == 2);
  BOOST_TEST(pet.getPhotoUrls()[0] == "url1");
  BOOST_TEST(pet.getPhotoUrls()[1] == "url2");
}

BOOST_AUTO_TEST_CASE(fromJsonWithTags) {
  std::string json_str = R"JSON(
{
    "id": "1",
    "name": "MyName",
    "tags": [
        {
            "id": "1",
            "name": "tag1"
        },
        {
            "id": "2",
            "name": "tag2"
        }
    ],
    "status": "available"
})JSON";

  Pet pet;
  pet.fromJsonString(json_str);

  BOOST_TEST(pet.getTags().size() == 2);
  BOOST_TEST(pet.getTags()[0]->getId() == 1);
  BOOST_TEST(pet.getTags()[1]->getId() == 2);
}

BOOST_DATA_TEST_CASE(validStatusValues,
                     boost::unit_test::data::make({"available", "pending",
                                                   "sold"}),
                     status) {
  Pet pet;
  pet.setStatus(status);

  BOOST_TEST(pet.getStatus() == status);
}

BOOST_DATA_TEST_CASE(invalidStatusValues,
                     boost::unit_test::data::make({"", "notallowed",
                                                   "not available"}),
                     invalid_status) {
  bool exceptionCaught = false;

  Pet pet;

  try {
    pet.setStatus(invalid_status);
  } catch (const std::runtime_error &excp) {
    exceptionCaught = true;
    const auto expectedErrorMessage =
        std::string("Value ") + invalid_status + " not allowed";
    BOOST_TEST(excp.what() == expectedErrorMessage);
  }
  BOOST_TEST(exceptionCaught);
}

BOOST_AUTO_TEST_SUITE_END()
