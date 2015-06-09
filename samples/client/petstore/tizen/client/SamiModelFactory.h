#ifndef ModelFactory_H_
#define ModelFactory_H_

#include "SamiObject.h"

#include "SamiUser.h"
#include "SamiCategory.h"
#include "SamiPet.h"
#include "SamiTag.h"
#include "SamiOrder.h"

namespace Swagger {
  void*
  create(String type) {
    if(type.Equals(L"SamiUser", true)) {
      return new SamiUser();
    }
    if(type.Equals(L"SamiCategory", true)) {
      return new SamiCategory();
    }
    if(type.Equals(L"SamiPet", true)) {
      return new SamiPet();
    }
    if(type.Equals(L"SamiTag", true)) {
      return new SamiTag();
    }
    if(type.Equals(L"SamiOrder", true)) {
      return new SamiOrder();
    }
    
    if(type.Equals(L"String", true)) {
      return new String();
    }
    if(type.Equals(L"Integer", true)) {
      return new Integer();
    }
    if(type.Equals(L"Long", true)) {
      return new Long();
    }
    if(type.Equals(L"DateTime", true)) {
      return new DateTime();
    }
    return null;
  }
} /* namespace Swagger */

#endif /* ModelFactory_H_ */
