/*
 * SamiCategory.h
 * 
 * 
 */

#ifndef SamiCategory_H_
#define SamiCategory_H_

#include <FApp.h>
#include <FBase.h>
#include <FSystem.h>
#include <FWebJson.h>
#include "SamiHelpers.h"
#include "SamiObject.h"

using namespace Tizen::Web::Json;


using Tizen::Base::Long;
using Tizen::Base::String;


namespace Swagger {

class SamiCategory: public SamiObject {
public:
    SamiCategory();
    SamiCategory(String* json);
    virtual ~SamiCategory();

    void init();

    void cleanup();

    String asJson ();

    JsonObject* asJsonObject();

    void fromJsonObject(IJsonValue* json);

    SamiCategory* fromJson(String* obj);

    
    Long* getPId();
    void setPId(Long* pId);
    
    String* getPName();
    void setPName(String* pName);
    

private:
    Long* pId;
    String* pName;
    
};

} /* namespace Swagger */

#endif /* SamiCategory_H_ */
