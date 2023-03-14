#define BOOST_TEST_INCLUDED
#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>

#include "model/User.h"

using namespace org::openapitools::server::model;


BOOST_AUTO_TEST_SUITE(UserModelTest)

BOOST_AUTO_TEST_CASE(toJsonString)
{
  User user;
  user.setId(4L);
  user.setUsername("User");
  user.setFirstName("First Name");
  user.setLastName("Last Name");
  user.setEmail("user@example.com");
  user.setPassword("passw0rd");
  user.setPhone("1234567890");
  user.setUserStatus(2);

  const auto json = user.toJsonString(true);

  User newUser;
  newUser.fromJsonString(json);

  BOOST_TEST(newUser.getId() == 4L);
  BOOST_TEST(newUser.getUsername() == "User");
  BOOST_TEST(newUser.getFirstName() == "First Name");
  BOOST_TEST(newUser.getLastName() == "Last Name");
  BOOST_TEST(newUser.getEmail() == "user@example.com");
  BOOST_TEST(newUser.getPassword() == "passw0rd");
  BOOST_TEST(newUser.getPhone() == "1234567890");
  BOOST_TEST(newUser.getUserStatus() == 2);

}

BOOST_AUTO_TEST_CASE(fromJsonString)
{
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

BOOST_AUTO_TEST_CASE(toAndFromPropertyTree)
{
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

  BOOST_TEST(newUser.getId() == 7L);
  BOOST_TEST(newUser.getUsername() == "User1");
  BOOST_TEST(newUser.getFirstName() == "First Name 1");
  BOOST_TEST(newUser.getLastName() == "Last Name 1");
  BOOST_TEST(newUser.getEmail() == "user1@example.com");
  BOOST_TEST(newUser.getPassword() == "passw0rd1");
  BOOST_TEST(newUser.getPhone() == "1111122222333");
  BOOST_TEST(newUser.getUserStatus() == 23);
}


BOOST_AUTO_TEST_CASE(fromJsonArrayString) {
  const std::string json = R"JSON(
[{
    "id": "4",
    "username": "TheUser",
    "firstName": "My Name",
    "lastName": "My Last Name",
    "email": "user@example.com",
    "password": "pa55word",
    "phone": "0987654321",
    "userStatus": "3"
},{
    "id": "3",
    "username": "TheUser2",
    "firstName": "My Name 2",
    "lastName": "My Last Name 2",
    "email": "user2@example.com",
    "password": "pa55word2",
    "phone": "098222222",
    "userStatus": "22"
},{
    "id": "1",
    "username": "TheUser3",
    "firstName": "My Name 3",
    "lastName": "My Last Name 3",
    "email": "user3@example.com",
    "password": "pa55word3",
    "phone": "098333333",
    "userStatus": "33"
}])JSON";

  const auto userVec = createUserVectorFromJsonString(json);

  BOOST_TEST(userVec.size() == 3);
  BOOST_TEST(userVec[0].getUsername() == "TheUser");
  BOOST_TEST(userVec[1].getUsername() == "TheUser2");
  BOOST_TEST(userVec[2].getUsername() == "TheUser3");

}

BOOST_AUTO_TEST_SUITE_END()
