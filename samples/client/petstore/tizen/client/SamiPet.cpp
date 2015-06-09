
#include "SamiPet.h"
#include <FLocales.h>

using namespace Tizen::Base;
using namespace Tizen::System;
using namespace Tizen::Base::Utility;
using namespace Tizen::Base::Collection;
using namespace Tizen::Web::Json;
using namespace Tizen::Locales;


namespace Swagger {

SamiPet::SamiPet() {
    init();
}

SamiPet::~SamiPet() {
    this->cleanup();
}

void
SamiPet::init() {
    pId = null;
    pCategory = null;
    pName = null;
    pPhotoUrls = null;
    pTags = null;
    pStatus = null;
    
}

void
SamiPet::cleanup() {
    if(pId != null) {
        
        delete pId;
        pId = null;
    }
    if(pCategory != null) {
        
        delete pCategory;
        pCategory = null;
    }
    if(pName != null) {
        
        delete pName;
        pName = null;
    }
    if(pPhotoUrls != null) {
        pPhotoUrls->RemoveAll(true);
        delete pPhotoUrls;
        pPhotoUrls = null;
    }
    if(pTags != null) {
        pTags->RemoveAll(true);
        delete pTags;
        pTags = null;
    }
    if(pStatus != null) {
        
        delete pStatus;
        pStatus = null;
    }
    
}


SamiPet*
SamiPet::fromJson(String* json) {
    this->cleanup();
    String str(json->GetPointer());
    int length = str.GetLength();

    ByteBuffer buffer;
    buffer.Construct(length);

    for (int i = 0; i < length; ++i) {
       byte b = str[i];
       buffer.SetByte(b);
    }

    IJsonValue* pJson = JsonParser::ParseN(buffer);
    fromJsonObject(pJson);
    if (pJson->GetType() == JSON_TYPE_OBJECT) {
       JsonObject* pObject = static_cast< JsonObject* >(pJson);
       pObject->RemoveAll(true);
    }
    else if (pJson->GetType() == JSON_TYPE_ARRAY) {
       JsonArray* pArray = static_cast< JsonArray* >(pJson);
       pArray->RemoveAll(true);
    }
    delete pJson;
    return this;
}


void
SamiPet::fromJsonObject(IJsonValue* pJson) {
    JsonObject* pJsonObject = static_cast< JsonObject* >(pJson);

    if(pJsonObject != null) {
        JsonString* pIdKey = new JsonString(L"id");
        IJsonValue* pIdVal = null;
        pJsonObject->GetValue(pIdKey, pIdVal);
        if(pIdVal != null) {
            
            pId = new Long();
            jsonToValue(pId, pIdVal, L"Long", L"Long");
        }
        delete pIdKey;
        JsonString* pCategoryKey = new JsonString(L"category");
        IJsonValue* pCategoryVal = null;
        pJsonObject->GetValue(pCategoryKey, pCategoryVal);
        if(pCategoryVal != null) {
            
            pCategory = new SamiCategory();
            jsonToValue(pCategory, pCategoryVal, L"SamiCategory", L"SamiCategory");
        }
        delete pCategoryKey;
        JsonString* pNameKey = new JsonString(L"name");
        IJsonValue* pNameVal = null;
        pJsonObject->GetValue(pNameKey, pNameVal);
        if(pNameVal != null) {
            
            pName = new String();
            jsonToValue(pName, pNameVal, L"String", L"String");
        }
        delete pNameKey;
        JsonString* pPhotoUrlsKey = new JsonString(L"photoUrls");
        IJsonValue* pPhotoUrlsVal = null;
        pJsonObject->GetValue(pPhotoUrlsKey, pPhotoUrlsVal);
        if(pPhotoUrlsVal != null) {
            pPhotoUrls = new ArrayList();
            
            jsonToValue(pPhotoUrls, pPhotoUrlsVal, L"IList", L"String");
        }
        delete pPhotoUrlsKey;
        JsonString* pTagsKey = new JsonString(L"tags");
        IJsonValue* pTagsVal = null;
        pJsonObject->GetValue(pTagsKey, pTagsVal);
        if(pTagsVal != null) {
            pTags = new ArrayList();
            
            jsonToValue(pTags, pTagsVal, L"IList", L"SamiTag");
        }
        delete pTagsKey;
        JsonString* pStatusKey = new JsonString(L"status");
        IJsonValue* pStatusVal = null;
        pJsonObject->GetValue(pStatusKey, pStatusVal);
        if(pStatusVal != null) {
            
            pStatus = new String();
            jsonToValue(pStatus, pStatusVal, L"String", L"String");
        }
        delete pStatusKey;
        
    }
}

SamiPet::SamiPet(String* json) {
    init();
    String str(json->GetPointer());
    int length = str.GetLength();

    ByteBuffer buffer;
    buffer.Construct(length);

    for (int i = 0; i < length; ++i) {
       byte b = str[i];
       buffer.SetByte(b);
    }

    IJsonValue* pJson = JsonParser::ParseN(buffer);
    fromJsonObject(pJson);
    if (pJson->GetType() == JSON_TYPE_OBJECT) {
       JsonObject* pObject = static_cast< JsonObject* >(pJson);
       pObject->RemoveAll(true);
    }
    else if (pJson->GetType() == JSON_TYPE_ARRAY) {
       JsonArray* pArray = static_cast< JsonArray* >(pJson);
       pArray->RemoveAll(true);
    }
    delete pJson;
}

String
SamiPet::asJson ()
{
    JsonObject* pJsonObject = asJsonObject();

    char *pComposeBuf = new char[256];
    JsonWriter::Compose(pJsonObject, pComposeBuf, 256);
    String s = String(pComposeBuf);

    delete pComposeBuf;
    pJsonObject->RemoveAll(true);
    delete pJsonObject;

    return s;
}

JsonObject*
SamiPet::asJsonObject() {
    JsonObject *pJsonObject = new JsonObject();
    pJsonObject->Construct();

    
    JsonString *pIdKey = new JsonString(L"id");
    pJsonObject->Add(pIdKey, toJson(getPId(), "Long", ""));

    
    JsonString *pCategoryKey = new JsonString(L"category");
    pJsonObject->Add(pCategoryKey, toJson(getPCategory(), "SamiCategory", ""));

    
    JsonString *pNameKey = new JsonString(L"name");
    pJsonObject->Add(pNameKey, toJson(getPName(), "String", ""));

    
    JsonString *pPhotoUrlsKey = new JsonString(L"photoUrls");
    pJsonObject->Add(pPhotoUrlsKey, toJson(getPPhotoUrls(), "String", "array"));

    
    JsonString *pTagsKey = new JsonString(L"tags");
    pJsonObject->Add(pTagsKey, toJson(getPTags(), "SamiTag", "array"));

    
    JsonString *pStatusKey = new JsonString(L"status");
    pJsonObject->Add(pStatusKey, toJson(getPStatus(), "String", ""));

    
    return pJsonObject;
}

Long*
SamiPet::getPId() {
    return pId;
}
void
SamiPet::setPId(Long* pId) {
    this->pId = pId;
}

SamiCategory*
SamiPet::getPCategory() {
    return pCategory;
}
void
SamiPet::setPCategory(SamiCategory* pCategory) {
    this->pCategory = pCategory;
}

String*
SamiPet::getPName() {
    return pName;
}
void
SamiPet::setPName(String* pName) {
    this->pName = pName;
}

IList*
SamiPet::getPPhotoUrls() {
    return pPhotoUrls;
}
void
SamiPet::setPPhotoUrls(IList* pPhotoUrls) {
    this->pPhotoUrls = pPhotoUrls;
}

IList*
SamiPet::getPTags() {
    return pTags;
}
void
SamiPet::setPTags(IList* pTags) {
    this->pTags = pTags;
}

String*
SamiPet::getPStatus() {
    return pStatus;
}
void
SamiPet::setPStatus(String* pStatus) {
    this->pStatus = pStatus;
}



} /* namespace Swagger */

