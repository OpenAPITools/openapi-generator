#ifndef ModelFactory_H_
#define ModelFactory_H_


#include "SWGApiResponse.h"
#include "SWGCategory.h"
#include "SWGOrder.h"
#include "SWGPet.h"
#include "SWGTag.h"
#include "SWGUser.h"

namespace Swagger {
  inline void* create(QString type) {
    if(QString("SWGApiResponse").compare(type) == 0) {
      return new SWGApiResponse();
    }
    if(QString("SWGCategory").compare(type) == 0) {
      return new SWGCategory();
    }
    if(QString("SWGOrder").compare(type) == 0) {
      return new SWGOrder();
    }
    if(QString("SWGPet").compare(type) == 0) {
      return new SWGPet();
    }
    if(QString("SWGTag").compare(type) == 0) {
      return new SWGTag();
    }
    if(QString("SWGUser").compare(type) == 0) {
      return new SWGUser();
    }
    
    return NULL;
  }

  inline void* create(QString json, QString type) {
    void* val = create(type);
    if(val != NULL) {
      SWGObject* obj = static_cast<SWGObject*>(val);
      return obj->fromJson(json);
    }
    if(type.startsWith("QString")) {
      return new QString();
    }
    return NULL;
  }
} /* namespace Swagger */

#endif /* ModelFactory_H_ */
