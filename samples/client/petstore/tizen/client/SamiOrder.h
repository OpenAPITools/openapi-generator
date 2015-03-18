/*
 * SamiOrder.h
 * 
 * 
 */

#ifndef SamiOrder_H_
#define SamiOrder_H_

#include <FApp.h>
#include <FBase.h>
#include <FSystem.h>
#include <FWebJson.h>
#include "SamiHelpers.h"
#include "SamiObject.h"

using namespace Tizen::Web::Json;


using Tizen::Base::Long;
using Tizen::Base::DateTime;
using Tizen::Base::String;
using Tizen::Base::Boolean;
using Tizen::Base::Integer;


namespace Swagger {

class SamiOrder: public SamiObject {
public:
    SamiOrder();
    SamiOrder(String* json);
    virtual ~SamiOrder();

    void init();

    void cleanup();

    String asJson ();

    JsonObject* asJsonObject();

    void fromJsonObject(IJsonValue* json);

    SamiOrder* fromJson(String* obj);

    
    Long* getpId();
    void setpId(Long* pId);
    
    Long* getpPetId();
    void setpPetId(Long* pPetId);
    
    Integer* getpQuantity();
    void setpQuantity(Integer* pQuantity);
    
    DateTime* getpShipDate();
    void setpShipDate(DateTime* pShipDate);
    
    String* getpStatus();
    void setpStatus(String* pStatus);
    
    Boolean* getpComplete();
    void setpComplete(Boolean* pComplete);
    

private:
    Long* pId;
    Long* pPetId;
    Integer* pQuantity;
    DateTime* pShipDate;
    String* pStatus;
    Boolean* pComplete;
    
};

} /* namespace Swagger */

#endif /* SamiOrder_H_ */
