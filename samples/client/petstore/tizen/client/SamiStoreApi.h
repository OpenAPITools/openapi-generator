#ifndef SamiStoreApi_H_
#define SamiStoreApi_H_

#include <FNet.h>
#include "SamiApiClient.h"
#include "SamiError.h"

using Tizen::Base::Integer;
using Tizen::Base::Long;
#include "SamiOrder.h"
using Tizen::Base::String;

using namespace Tizen::Net::Http;

namespace Swagger {

class SamiStoreApi {
public:
  SamiStoreApi();
  virtual ~SamiStoreApi();

  void 
  deleteOrderWithCompletion(String* orderId, void(* handler)(SamiError*));
  HashMap* 
  getInventoryWithCompletion( void (* handler)(HashMap*, SamiError*));
  SamiOrder* 
  getOrderByIdWithCompletion(Long* orderId, void (* handler)(SamiOrder*, SamiError*));
  SamiOrder* 
  placeOrderWithCompletion(SamiOrder* body, void (* handler)(SamiOrder*, SamiError*));
  static String getBasePath() {
    return L"http://petstore.swagger.io/v2";
  }

private:
  SamiApiClient* client;
};


} /* namespace Swagger */

#endif /* SamiStoreApi_H_ */
