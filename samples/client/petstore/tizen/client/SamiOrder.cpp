
#include "SamiOrder.h"
#include <FLocales.h>

using namespace Tizen::Base;
using namespace Tizen::System;
using namespace Tizen::Base::Utility;
using namespace Tizen::Base::Collection;
using namespace Tizen::Web::Json;
using namespace Tizen::Locales;


namespace Swagger {

SamiOrder::SamiOrder() {
    init();
}

SamiOrder::~SamiOrder() {
    this->cleanup();
}

void
SamiOrder::init() {
    pId = null;
    pPetId = null;
    pQuantity = null;
    pShipDate = null;
    pStatus = null;
    pComplete = null;
    
}

void
SamiOrder::cleanup() {
    if(pId != null) {
        
        delete pId;
        pId = null;
    }
    if(pPetId != null) {
        
        delete pPetId;
        pPetId = null;
    }
    if(pQuantity != null) {
        
        delete pQuantity;
        pQuantity = null;
    }
    if(pShipDate != null) {
        
        delete pShipDate;
        pShipDate = null;
    }
    if(pStatus != null) {
        
        delete pStatus;
        pStatus = null;
    }
    if(pComplete != null) {
        
        delete pComplete;
        pComplete = null;
    }
    
}


SamiOrder*
SamiOrder::fromJson(String* json) {
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
SamiOrder::fromJsonObject(IJsonValue* pJson) {
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
        JsonString* pPetIdKey = new JsonString(L"petId");
        IJsonValue* pPetIdVal = null;
        pJsonObject->GetValue(pPetIdKey, pPetIdVal);
        if(pPetIdVal != null) {
            
            pPetId = new Long();
            jsonToValue(pPetId, pPetIdVal, L"Long", L"Long");
        }
        delete pPetIdKey;
        JsonString* pQuantityKey = new JsonString(L"quantity");
        IJsonValue* pQuantityVal = null;
        pJsonObject->GetValue(pQuantityKey, pQuantityVal);
        if(pQuantityVal != null) {
            
            pQuantity = new Integer();
            jsonToValue(pQuantity, pQuantityVal, L"Integer", L"Integer");
        }
        delete pQuantityKey;
        JsonString* pShipDateKey = new JsonString(L"shipDate");
        IJsonValue* pShipDateVal = null;
        pJsonObject->GetValue(pShipDateKey, pShipDateVal);
        if(pShipDateVal != null) {
            
            pShipDate = new DateTime();
            jsonToValue(pShipDate, pShipDateVal, L"DateTime", L"DateTime");
        }
        delete pShipDateKey;
        JsonString* pStatusKey = new JsonString(L"status");
        IJsonValue* pStatusVal = null;
        pJsonObject->GetValue(pStatusKey, pStatusVal);
        if(pStatusVal != null) {
            
            pStatus = new String();
            jsonToValue(pStatus, pStatusVal, L"String", L"String");
        }
        delete pStatusKey;
        JsonString* pCompleteKey = new JsonString(L"complete");
        IJsonValue* pCompleteVal = null;
        pJsonObject->GetValue(pCompleteKey, pCompleteVal);
        if(pCompleteVal != null) {
            
            pComplete = new Boolean(false);
            jsonToValue(pComplete, pCompleteVal, L"Boolean", L"Boolean");
        }
        delete pCompleteKey;
        
    }
}

SamiOrder::SamiOrder(String* json) {
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
SamiOrder::asJson ()
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
SamiOrder::asJsonObject() {
    JsonObject *pJsonObject = new JsonObject();
    pJsonObject->Construct();

    
    JsonString *pIdKey = new JsonString(L"id");
    pJsonObject->Add(pIdKey, toJson(getPId(), "Long", ""));

    
    JsonString *pPetIdKey = new JsonString(L"petId");
    pJsonObject->Add(pPetIdKey, toJson(getPPetId(), "Long", ""));

    
    JsonString *pQuantityKey = new JsonString(L"quantity");
    pJsonObject->Add(pQuantityKey, toJson(getPQuantity(), "Integer", ""));

    
    JsonString *pShipDateKey = new JsonString(L"shipDate");
    pJsonObject->Add(pShipDateKey, toJson(getPShipDate(), "DateTime", ""));

    
    JsonString *pStatusKey = new JsonString(L"status");
    pJsonObject->Add(pStatusKey, toJson(getPStatus(), "String", ""));

    
    JsonString *pCompleteKey = new JsonString(L"complete");
    pJsonObject->Add(pCompleteKey, toJson(getPComplete(), "Boolean", ""));

    
    return pJsonObject;
}

Long*
SamiOrder::getPId() {
    return pId;
}
void
SamiOrder::setPId(Long* pId) {
    this->pId = pId;
}

Long*
SamiOrder::getPPetId() {
    return pPetId;
}
void
SamiOrder::setPPetId(Long* pPetId) {
    this->pPetId = pPetId;
}

Integer*
SamiOrder::getPQuantity() {
    return pQuantity;
}
void
SamiOrder::setPQuantity(Integer* pQuantity) {
    this->pQuantity = pQuantity;
}

DateTime*
SamiOrder::getPShipDate() {
    return pShipDate;
}
void
SamiOrder::setPShipDate(DateTime* pShipDate) {
    this->pShipDate = pShipDate;
}

String*
SamiOrder::getPStatus() {
    return pStatus;
}
void
SamiOrder::setPStatus(String* pStatus) {
    this->pStatus = pStatus;
}

Boolean*
SamiOrder::getPComplete() {
    return pComplete;
}
void
SamiOrder::setPComplete(Boolean* pComplete) {
    this->pComplete = pComplete;
}



} /* namespace Swagger */

