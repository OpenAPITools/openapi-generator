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

    
    Long* getId();
    void setId(Long* pId);
    
    SamiCategory* getCategory();
    void setCategory(SamiCategory* pCategory);
    
    String* getName();
    void setName(String* pName);
    
    IList* getPhotoUrls();
    void setPhotoUrls(IList* pPhotoUrls);
    
    IList* getTags();
    void setTags(IList* pTags);
    
    String* getStatus();
    void setStatus(String* pStatus);
    

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
