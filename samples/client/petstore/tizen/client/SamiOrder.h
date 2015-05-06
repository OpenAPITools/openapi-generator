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

    
    Long* getPId();
    void setPId(Long* pId);
    
    Long* getPPetId();
    void setPPetId(Long* pPetId);
    
    Integer* getPQuantity();
    void setPQuantity(Integer* pQuantity);
    
    DateTime* getPShipDate();
    void setPShipDate(DateTime* pShipDate);
    
    String* getPStatus();
    void setPStatus(String* pStatus);
    
    Boolean* getPComplete();
    void setPComplete(Boolean* pComplete);
    

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
