#define BOOST_TEST_INCLUDED

#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/beast/http/verb.hpp>
#include <chrono>
#include <thread>

#include "api/StoreApi.h"

#include "beast_client.h"


using namespace org::openapitools::server::model;
using namespace org::openapitools::server::api;

class MyStoreApiStoreInventoryResource : public StoreApiResources::StoreInventoryResource {
public:
  virtual std::pair<int, std::map<std::string, int32_t>> handler_GET() override {
      std::map<std::string, int32_t> map;
      map["Hello inventory"] = 23;
      return std::make_pair<int, decltype(map)>(200, std::move(map));
  }

};

std::string formatJson(const std::string& jsonString) {
  boost::property_tree::ptree pt;
  std::stringstream sstream(jsonString);
  read_json(sstream,pt);
  write_json(sstream, pt);
  return sstream.str();
}

BOOST_AUTO_TEST_SUITE(StoreApiTest)

BOOST_AUTO_TEST_CASE(startService)
{
  auto restbedService = std::make_shared<restbed::Service>();

  auto storeApi = StoreApi(restbedService);
  std::shared_ptr<StoreApiResources::StoreInventoryResource> res = std::make_shared<MyStoreApiStoreInventoryResource>();
  storeApi.setResource(res);

  std::thread thread([&]{
    auto settings = std::make_shared<restbed::Settings>();
    settings->set_port(1234);
    restbedService->start(settings);
  });
  thread.detach();

  sleep(1);

  int status = 0;
  std::string data;

  // localhost:1234/v2/store/inventory/
  auto response = requestData(boost::beast::http::verb::get,
                          "/v2/store/inventory/");
  status = response.first;
  data = formatJson(response.second);

  const auto expectedJson = formatJson(R"JSON({"Hello inventory": "23"})JSON");
  BOOST_TEST(200 == status);
  BOOST_TEST(expectedJson == data);

  // /store/order/{orderId: .*}/
  response = requestData(boost::beast::http::verb::get,
                                     "/v2/store/order/3");

  status = response.first;
  data = response.second;

  BOOST_TEST(404 == status);
  BOOST_TEST(data.empty());

  restbedService->stop();
}

BOOST_AUTO_TEST_SUITE_END()