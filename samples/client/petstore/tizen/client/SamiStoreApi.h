#ifndef SamiStoreApi_H_
#define SamiStoreApi_H_

#include <FNet.h>
#include "SamiApiClient.h"
#include "SamiError.h"

using Tizen::Base::Integer;
#include "SamiOrder.h"
using Tizen::Base::String;

using namespace Tizen::Net::Http;

namespace Swagger {

class SamiStoreApi {
public:
  SamiStoreApi();
  virtual ~SamiStoreApi();

  
  HashMap* 
  getInventoryWithCompletion( void (* handler)(HashMap*, SamiError*));
  
  SamiOrder* 
  placeOrderWithCompletion(SamiOrder* body, void (* handler)(SamiOrder*, SamiError*));
  
  SamiOrder* 
  getOrderByIdWithCompletion(String* orderId, void (* handler)(SamiOrder*, SamiError*));
  
  void 
  deleteOrderWithCompletion(String* orderId, void(* handler)(SamiError*));
  
  static String getBasePath() {
    return L"http://petstore.swagger.io/v2";
  }

private:
  SamiApiClient* client;
};


} /* namespace Swagger */

#endif /* SamiStoreApi_H_ */
