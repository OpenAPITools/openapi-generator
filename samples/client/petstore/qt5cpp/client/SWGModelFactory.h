#ifndef ModelFactory_H_
#define ModelFactory_H_


#include "SWGUser.h"
#include "SWGCategory.h"
#include "SWGPet.h"
#include "SWGTag.h"
#include "SWGOrder.h"

namespace Swagger {
  void*
  create(QString type) {
    if(QString("SWGUser").compare(type) == 0) {
      return new SWGUser();
    }
    if(QString("SWGCategory").compare(type) == 0) {
      return new SWGCategory();
    }
    if(QString("SWGPet").compare(type) == 0) {
      return new SWGPet();
    }
    if(QString("SWGTag").compare(type) == 0) {
      return new SWGTag();
    }
    if(QString("SWGOrder").compare(type) == 0) {
      return new SWGOrder();
    }
    
    return NULL;
  }
} /* namespace Swagger */

#endif /* ModelFactory_H_ */
