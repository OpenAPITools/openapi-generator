/*
 * SamiPet.h
 * 
 * 
 */

#ifndef SamiPet_H_
#define SamiPet_H_

#include <FApp.h>
#include <FBase.h>
#include <FSystem.h>
#include <FWebJson.h>
#include "SamiHelpers.h"
#include "SamiObject.h"

using namespace Tizen::Web::Json;


#include "SamiCategory.h"
using Tizen::Base::Long;
using Tizen::Base::String;
#include "SamiTag.h"
using Tizen::Base::Collection::IList;


namespace Swagger {

class SamiPet: public SamiObject {
public:
    SamiPet();
    SamiPet(String* json);
    virtual ~SamiPet();

    void init();

    void cleanup();

    String asJson ();

    JsonObject* asJsonObject();

    void fromJsonObject(IJsonValue* json);

    SamiPet* fromJson(String* obj);

    
    Long* getPId();
    void setPId(Long* pId);
    
    SamiCategory* getPCategory();
    void setPCategory(SamiCategory* pCategory);
    
    String* getPName();
    void setPName(String* pName);
    
    IList* getPPhotoUrls();
    void setPPhotoUrls(IList* pPhotoUrls);
    
    IList* getPTags();
    void setPTags(IList* pTags);
    
    String* getPStatus();
    void setPStatus(String* pStatus);
    

private:
    Long* pId;
    SamiCategory* pCategory;
    String* pName;
    IList* pPhotoUrls;
    IList* pTags;
    String* pStatus;
    
};

} /* namespace Swagger */

#endif /* SamiPet_H_ */
