#ifndef SamiPetApi_H_
#define SamiPetApi_H_

#include <FNet.h>
#include "SamiApiClient.h"
#include "SamiError.h"

#include "SamiPet.h"
using Tizen::Base::String;
using Tizen::Base::Long;
#include "SamiFile.h"

using namespace Tizen::Net::Http;

namespace Swagger {

class SamiPetApi {
public:
  SamiPetApi();
  virtual ~SamiPetApi();

  
  void 
  updatePetWithCompletion(SamiPet* body, void(* handler)(SamiError*));
  
  void 
  addPetWithCompletion(SamiPet* body, void(* handler)(SamiError*));
  
  IList* 
  findPetsByStatusWithCompletion(IList* status, void (* handler)(IList*, SamiError*));
  
  IList* 
  findPetsByTagsWithCompletion(IList* tags, void (* handler)(IList*, SamiError*));
  
  SamiPet* 
  getPetByIdWithCompletion(Long* petId, void (* handler)(SamiPet*, SamiError*));
  
  void 
  updatePetWithFormWithCompletion(String* petId, String* name, String* status, void(* handler)(SamiError*));
  
  void 
  deletePetWithCompletion(Long* petId, String* apiKey, void(* handler)(SamiError*));
  
  void 
  uploadFileWithCompletion(Long* petId, String* additionalMetadata, SamiFile* file, void(* handler)(SamiError*));
  
  static String getBasePath() {
    return L"http://petstore.swagger.io/v2";
  }

private:
  SamiApiClient* client;
};


} /* namespace Swagger */

#endif /* SamiPetApi_H_ */
