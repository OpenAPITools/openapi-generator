#define BOOST_TEST_INCLUDED
#include <list>
#include <boost/property_tree/ptree.hpp>
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "../ApprovalTests.hpp"

#include "model/ApiResponse.h"

using namespace ApprovalTests;
using namespace org::openapitools::client::model;

BOOST_AUTO_TEST_SUITE(ApiResponseModelTest)

BOOST_AUTO_TEST_CASE(toJsonString) {
  ApiResponse apiResponse;
  apiResponse.setCode(404);
  apiResponse.setType("Error");
  apiResponse.setMessage("Not available");

  Approvals::verify(apiResponse.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(fromJsonString) {
  const std::string json = R"JSON(
{
    "code": "200",
    "type": "Ok",
    "message": "Nice!"
}
)JSON";

  ApiResponse apiResponse;

  apiResponse.fromJsonString(json);

  BOOST_TEST(apiResponse.getCode() == 200);
  BOOST_TEST(apiResponse.getType() == "Ok");
  BOOST_TEST(apiResponse.getMessage() == "Nice!");
}

BOOST_AUTO_TEST_CASE(test_createJsonStringFromApiResponseVector) {
  auto resp0 = std::make_shared<ApiResponse>();
  auto resp1 = std::make_shared<ApiResponse>();

  resp0->setCode(200);
  resp0->setType("Ok");
  resp0->setMessage("Nice!");
  resp1->setCode(404);
  resp1->setType("error");
  resp1->setMessage("bad!");

  auto vec = std::vector<std::shared_ptr<ApiResponse>>();
  vec.emplace_back(resp0);
  vec.emplace_back(resp1);

  const auto json = createJsonStringFromModelVector(vec);
    
  Approvals::verify(json);
}

BOOST_AUTO_TEST_CASE(toAndFromPropertyTree) {
  ApiResponse apiResponse;
  apiResponse.setCode(200);
  apiResponse.setType("Ack");
  apiResponse.setMessage("cool");

  const auto pt = apiResponse.toPropertyTree();

  const auto newApiResponse = ApiResponse(pt);

  BOOST_TEST(newApiResponse.getCode() == 200);
  BOOST_TEST(newApiResponse.getType() == "Ack");
  BOOST_TEST(newApiResponse.getMessage() == "cool");
}

BOOST_AUTO_TEST_SUITE_END()
