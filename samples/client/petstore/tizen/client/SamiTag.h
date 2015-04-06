/*
 * SamiTag.h
 * 
 * 
 */

#ifndef SamiTag_H_
#define SamiTag_H_

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

class SamiTag: public SamiObject {
public:
    SamiTag();
    SamiTag(String* json);
    virtual ~SamiTag();

    void init();

    void cleanup();

    String asJson ();

    JsonObject* asJsonObject();

    void fromJsonObject(IJsonValue* json);

    SamiTag* fromJson(String* obj);

    
    Long* getPId();
    void setPId(Long* pId);
    
    String* getPName();
    void setPName(String* pName);
    

private:
    Long* pId;
    String* pName;
    
};

} /* namespace Swagger */

#endif /* SamiTag_H_ */
