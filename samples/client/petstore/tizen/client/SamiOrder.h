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

    
    Long* getId();
    void setId(Long* pId);
    
    Long* getPetId();
    void setPetId(Long* pPetId);
    
    Integer* getQuantity();
    void setQuantity(Integer* pQuantity);
    
    DateTime* getShipDate();
    void setShipDate(DateTime* pShipDate);
    
    String* getStatus();
    void setStatus(String* pStatus);
    
    Boolean* getComplete();
    void setComplete(Boolean* pComplete);
    

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
