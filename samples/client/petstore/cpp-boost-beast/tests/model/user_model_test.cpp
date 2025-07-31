#define BOOST_TEST_INCLUDED
#include <list>
#include <boost/property_tree/ptree.hpp>
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "../ApprovalTests.hpp"

#include "model/User.h"

using namespace ApprovalTests;
using namespace org::openapitools::client::model;

BOOST_AUTO_TEST_SUITE(UserModelTest)

BOOST_AUTO_TEST_CASE(toJsonString) {
  User user;
  user.setId(4L);
  user.setUsername("User");
  user.setFirstName("First Name");
  user.setLastName("Last Name");
  user.setEmail("user@example.com");
  user.setPassword("passw0rd");
  user.setPhone("1234567890");
  user.setUserStatus(2);

  Approvals::verify(user.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(fromJsonString) {
  const std::string json = R"JSON(
{
    "id": "3",
    "username": "TheUser",
    "firstName": "My Name",
    "lastName": "My Last Name",
    "email": "user@example.com",
    "password": "pa55word",
    "phone": "0987654321",
    "userStatus": "3"
})JSON";

  User user;
  user.fromJsonString(json);

  BOOST_TEST(user.getId() == 3L);
  BOOST_TEST(user.getUsername() == "TheUser");
  BOOST_TEST(user.getFirstName() == "My Name");
  BOOST_TEST(user.getLastName() == "My Last Name");
  BOOST_TEST(user.getEmail() == "user@example.com");
  BOOST_TEST(user.getPassword() == "pa55word");
  BOOST_TEST(user.getPhone() == "0987654321");
  BOOST_TEST(user.getUserStatus() == 3);
}

BOOST_AUTO_TEST_CASE(toAndFromPropertyTree) {
  User user;
  user.setId(7L);
  user.setUsername("User1");
  user.setFirstName("First Name 1");
  user.setLastName("Last Name 1");
  user.setEmail("user1@example.com");
  user.setPassword("passw0rd1");
  user.setPhone("1111122222333");
  user.setUserStatus(23);

  const auto pt = user.toPropertyTree();

  const auto newUser = User(pt);

  Approvals::verify(user.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(test_createJsonStringFromUserVector) {
  auto user0 = std::make_shared<User>();
  auto user1 = std::make_shared<User>();
  auto user2 = std::make_shared<User>();

  user0->setId(4);
  user0->setUsername("TheUser");
  user0->setFirstName("My Name");
  user0->setLastName("My Last Name");
  user0->setEmail("user@example.com");
  user0->setPassword("pa55word");
  user0->setPhone("0987654321");
  user0->setUserStatus(3);

  user1->setId(3);
  user1->setUsername("TheUser2");
  user1->setFirstName("My Name 2");
  user1->setLastName("My Last Name 2");
  user1->setEmail("user2@example.com");
  user1->setPassword("pa55word2");
  user1->setPhone("098222222");
  user1->setUserStatus(22);

  user2->setId(1);
  user2->setUsername("TheUser3");
  user2->setFirstName("My Name 3");
  user2->setLastName("My Last Name 3");
  user2->setEmail("user3@example.com");
  user2->setPassword("pa55word3");
  user2->setPhone("098333333");
  user2->setUserStatus(33);

  auto vec = std::vector<std::shared_ptr<User>>();
  vec.emplace_back(user0);
  vec.emplace_back(user1);
  vec.emplace_back(user2);

  const auto json = createJsonStringFromModelVector(vec);

  Approvals::verify(json);
  
}

BOOST_AUTO_TEST_SUITE_END()
