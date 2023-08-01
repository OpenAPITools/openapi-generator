#define BOOST_TEST_INCLUDED
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "model/ApiResponse.h"

using namespace org::openapitools::server::model;


BOOST_AUTO_TEST_SUITE(ApiResponseModelTest)

BOOST_AUTO_TEST_CASE(toJsonString)
{
  ApiResponse apiResponse;
  apiResponse.setCode(404);
  apiResponse.setType("Error");
  apiResponse.setMessage("Not available");

  const auto json = apiResponse.toJsonString(true);

  ApiResponse newResponse;
  newResponse.fromJsonString(json);

  BOOST_TEST(newResponse.getCode() == 404);
  BOOST_TEST(newResponse.getType() == "Error");
  BOOST_TEST(newResponse.getMessage() == "Not available");


}

BOOST_AUTO_TEST_CASE(fromJsonString)
{
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


BOOST_AUTO_TEST_CASE(fromJsonArrayString)
{
  const std::string json = R"JSON(
[{
    "code": "200",
    "type": "Ok",
    "message": "Nice!"
},{
    "code": "404",
    "type": "error",
    "message": "bad!"
}]
)JSON";

  const auto apiResponseVec = createApiResponseVectorFromJsonString(json);

  BOOST_TEST(apiResponseVec.size() == 2);
  BOOST_TEST(apiResponseVec[0].getCode() == 200);
  BOOST_TEST(apiResponseVec[0].getType() == "Ok");
  BOOST_TEST(apiResponseVec[0].getMessage() == "Nice!");
  BOOST_TEST(apiResponseVec[1].getCode() == 404);
  BOOST_TEST(apiResponseVec[1].getType() == "error");
  BOOST_TEST(apiResponseVec[1].getMessage() == "bad!");
}

BOOST_AUTO_TEST_CASE(toAndFromPropertyTree)
{
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
