/*
 * SamiApiResponse.h
 * 
 * Describes the result of uploading an image resource
 */

#ifndef SamiApiResponse_H_
#define SamiApiResponse_H_

#include <FApp.h>
#include <FBase.h>
#include <FSystem.h>
#include <FWebJson.h>
#include "SamiHelpers.h"
#include "SamiObject.h"

using namespace Tizen::Web::Json;


using Tizen::Base::Integer;
using Tizen::Base::String;


namespace Swagger {

class SamiApiResponse: public SamiObject {
public:
    SamiApiResponse();
    SamiApiResponse(String* json);
    virtual ~SamiApiResponse();

    void init();

    void cleanup();

    String asJson ();

    JsonObject* asJsonObject();

    void fromJsonObject(IJsonValue* json);

    SamiApiResponse* fromJson(String* obj);

    Integer* getPCode();
    void setPCode(Integer* pCode);
    String* getPType();
    void setPType(String* pType);
    String* getPMessage();
    void setPMessage(String* pMessage);

private:
    Integer* pCode;
String* pType;
String* pMessage;
};

} /* namespace Swagger */

#endif /* SamiApiResponse_H_ */
