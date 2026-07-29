#define BOOST_TEST_INCLUDED
#include "../ApprovalTests.hpp"
#include <boost/json.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>

#include "model/Category.h"
#include "model/Pet.h"
#include "model/Tag.h"

using namespace ApprovalTests;
using namespace org::openapitools::client::model;

BOOST_AUTO_TEST_SUITE(PetModelTest)

BOOST_AUTO_TEST_CASE(toJsonString) {
  Pet pet;
  pet.setId(1);
  pet.setName("MyName");
  Approvals::verify(pet.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(requiredAndOptionalPropertyPresence) {
  Pet pet;

  const auto initialValue = pet.toJsonValue();
  const auto &initialObject = initialValue.as_object();
  BOOST_TEST(initialObject.find("name") != initialObject.end());
  BOOST_TEST(initialObject.find("photoUrls") != initialObject.end());
  BOOST_TEST(initialObject.find("id") == initialObject.end());
  BOOST_TEST(initialObject.find("category") == initialObject.end());
  BOOST_TEST(initialObject.find("tags") == initialObject.end());
  BOOST_TEST(initialObject.find("status") == initialObject.end());

  pet.setId(0);
  pet.setCategory(Category());
  pet.setTags(std::vector<std::shared_ptr<Tag>>{});

  const auto updatedValue = pet.toJsonValue();
  const auto &updatedObject = updatedValue.as_object();
  BOOST_TEST(updatedObject.at("id").as_int64() == 0);
  BOOST_TEST(updatedObject.at("category").is_object());
  BOOST_TEST(updatedObject.at("tags").as_array().empty());
}

BOOST_AUTO_TEST_CASE(toJsonStringWithCategory) {
  Pet pet;
  pet.setId(1);
  pet.setName("MyName");
  Category category;
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
    "id": 23,
    "name": "ThePet",
    "photoUrls": [],
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

  const auto jsonVector  = createJsonStringFromModelVector(vector);

  Approvals::verify(jsonVector);
}

BOOST_AUTO_TEST_CASE(fromJsonAndPropertyTree) {
  const std::string json = R"JSON(
{
    "id": 23,
    "name": "ThePet",
    "photoUrls": [],
    "status": "available"
})JSON";

  Pet pet{boost::json::parse(json)};

  BOOST_TEST(pet.getId() == 23);
  BOOST_TEST(pet.getName() == "ThePet");
  BOOST_TEST(pet.getStatus() == "available");
}

BOOST_AUTO_TEST_CASE(fromPropertyTree) {
  boost::json::object object;
  object["id"] = 11;
  object["status"] = "available";
  object["name"] = "Fluffy";

  boost::json::array tags;
  boost::json::object tag1;
  tag1["name"] = "tag1";
  tag1["id"] = 1;
  tags.emplace_back(std::move(tag1));
  boost::json::object tag2;
  tag2["name"] = "tag2";
  tag2["id"] = 2;
  tags.emplace_back(std::move(tag2));
  object["tags"] = std::move(tags);

  boost::json::array photoUrls;
  photoUrls.emplace_back("www.example.com/photo1");
  photoUrls.emplace_back("www.example.com/photo2");
  object["photoUrls"] = std::move(photoUrls);

  boost::json::object category;
  category["name"] = "Category1";
  category["id"] = 0;
  object["category"] = std::move(category);

  Pet pet;
  pet.fromJsonValue(object);
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

  Category category;
  category.setName("Category1");
  pet.setCategory(category);

  const auto value = pet.toJsonValue();
  const auto& object = value.as_object();

  BOOST_TEST(object.find("id") == object.end());
  BOOST_TEST(object.at("name").as_string() == "");
  BOOST_TEST(object.find("status") == object.end());

  const auto& tagsFromJson = object.at("tags").as_array();
  BOOST_TEST(tagsFromJson.size() == 1);

  const auto& photoUrlsFromJson = object.at("photoUrls").as_array();
  BOOST_TEST(photoUrlsFromJson.size() == 2);

  BOOST_TEST(pet.getCategory().getName() == "Category1");
  const auto& categoryFromJson = object.at("category").as_object();
  BOOST_TEST(categoryFromJson.at("name").as_string() == "Category1");
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
    "id": 1,
    "name": "MyName",
    "photoUrls": [],
    "tags": [
        {
            "id": 1,
            "name": "tag1"
        },
        {
            "id": 2,
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
    const auto expectedErrorMessage = std::string("Value not allowed");
    BOOST_TEST(excp.what() == expectedErrorMessage);
  }
  BOOST_TEST(exceptionCaught);
}

BOOST_AUTO_TEST_CASE(invalidStatusJsonUsesSetterValidation) {
  Pet pet;

  BOOST_CHECK_EXCEPTION(
      pet.fromJsonString(R"JSON({"name":"Fluffy","photoUrls":[],"status":"notallowed"})JSON"),
      std::invalid_argument, [](const std::invalid_argument &exception) {
        return std::string(exception.what()) == "Decode failed for 'status' in Pet: Value not allowed";
      });
}

BOOST_AUTO_TEST_SUITE_END()
