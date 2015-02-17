#ifndef SamiUserApi_H_
#define SamiUserApi_H_

#include <FNet.h>
#include "SamiApiClient.h"
#include "SamiError.h"

#include "SamiUser.h"
using Tizen::Base::Collection::IList;
using Tizen::Base::String;

using namespace Tizen::Net::Http;

namespace Swagger {

class SamiUserApi {
public:
  SamiUserApi();
  virtual ~SamiUserApi();

  
  void 
  createUserWithCompletion(SamiUser* body, void(* handler)(SamiError*));
  
  void 
  createUsersWithArrayInputWithCompletion(IList* body, void(* handler)(SamiError*));
  
  void 
  createUsersWithListInputWithCompletion(IList* body, void(* handler)(SamiError*));
  
  String* 
  loginUserWithCompletion(String* username, String* password, void (* handler)(String*, SamiError*));
  
  void 
  logoutUserWithCompletion( void(* handler)(SamiError*));
  
  SamiUser* 
  getUserByNameWithCompletion(String* username, void (* handler)(SamiUser*, SamiError*));
  
  void 
  updateUserWithCompletion(String* username, SamiUser* body, void(* handler)(SamiError*));
  
  void 
  deleteUserWithCompletion(String* username, void(* handler)(SamiError*));
  
  static String getBasePath() {
    return L"http://petstore.swagger.io/v2";
  }

private:
  SamiApiClient* client;
};


} /* namespace Swagger */

#endif /* SamiUserApi_H_ */
