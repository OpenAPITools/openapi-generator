#ifndef _Sami_OBJECT_H_
#define _Sami_OBJECT_H_

#include <FNet.h>
#include <FWebJson.h>
#include <FBase.h>

using Tizen::Base::String;

using namespace Tizen::Web::Json;

class SamiObject {
  public:
    virtual JsonObject* asJsonObject() {
      return null;
    }
    virtual ~SamiObject() {}
    virtual SamiObject* fromJson(String* obj) {
      return null;
    }
    virtual void fromJsonObject(IJsonValue* obj) {}
    virtual String asJson() {
      return L"";
    }
};

#endif /* _Sami_OBJECT_H_ */
