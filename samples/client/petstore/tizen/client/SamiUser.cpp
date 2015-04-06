
#include "SamiUser.h"
#include <FLocales.h>

using namespace Tizen::Base;
using namespace Tizen::System;
using namespace Tizen::Base::Utility;
using namespace Tizen::Base::Collection;
using namespace Tizen::Web::Json;
using namespace Tizen::Locales;


namespace Swagger {

SamiUser::SamiUser() {
    init();
}

SamiUser::~SamiUser() {
    this->cleanup();
}

void
SamiUser::init() {
    pId = null;
    pUsername = null;
    pFirstName = null;
    pLastName = null;
    pEmail = null;
    pPassword = null;
    pPhone = null;
    pUserStatus = null;
    
}

void
SamiUser::cleanup() {
    if(pId != null) {
        
        delete pId;
        pId = null;
    }
    if(pUsername != null) {
        
        delete pUsername;
        pUsername = null;
    }
    if(pFirstName != null) {
        
        delete pFirstName;
        pFirstName = null;
    }
    if(pLastName != null) {
        
        delete pLastName;
        pLastName = null;
    }
    if(pEmail != null) {
        
        delete pEmail;
        pEmail = null;
    }
    if(pPassword != null) {
        
        delete pPassword;
        pPassword = null;
    }
    if(pPhone != null) {
        
        delete pPhone;
        pPhone = null;
    }
    if(pUserStatus != null) {
        
        delete pUserStatus;
        pUserStatus = null;
    }
    
}


SamiUser*
SamiUser::fromJson(String* json) {
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
SamiUser::fromJsonObject(IJsonValue* pJson) {
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
        JsonString* pUsernameKey = new JsonString(L"username");
        IJsonValue* pUsernameVal = null;
        pJsonObject->GetValue(pUsernameKey, pUsernameVal);
        if(pUsernameVal != null) {
            
            pUsername = new String();
            jsonToValue(pUsername, pUsernameVal, L"String", L"String");
        }
        delete pUsernameKey;
        JsonString* pFirstNameKey = new JsonString(L"firstName");
        IJsonValue* pFirstNameVal = null;
        pJsonObject->GetValue(pFirstNameKey, pFirstNameVal);
        if(pFirstNameVal != null) {
            
            pFirstName = new String();
            jsonToValue(pFirstName, pFirstNameVal, L"String", L"String");
        }
        delete pFirstNameKey;
        JsonString* pLastNameKey = new JsonString(L"lastName");
        IJsonValue* pLastNameVal = null;
        pJsonObject->GetValue(pLastNameKey, pLastNameVal);
        if(pLastNameVal != null) {
            
            pLastName = new String();
            jsonToValue(pLastName, pLastNameVal, L"String", L"String");
        }
        delete pLastNameKey;
        JsonString* pEmailKey = new JsonString(L"email");
        IJsonValue* pEmailVal = null;
        pJsonObject->GetValue(pEmailKey, pEmailVal);
        if(pEmailVal != null) {
            
            pEmail = new String();
            jsonToValue(pEmail, pEmailVal, L"String", L"String");
        }
        delete pEmailKey;
        JsonString* pPasswordKey = new JsonString(L"password");
        IJsonValue* pPasswordVal = null;
        pJsonObject->GetValue(pPasswordKey, pPasswordVal);
        if(pPasswordVal != null) {
            
            pPassword = new String();
            jsonToValue(pPassword, pPasswordVal, L"String", L"String");
        }
        delete pPasswordKey;
        JsonString* pPhoneKey = new JsonString(L"phone");
        IJsonValue* pPhoneVal = null;
        pJsonObject->GetValue(pPhoneKey, pPhoneVal);
        if(pPhoneVal != null) {
            
            pPhone = new String();
            jsonToValue(pPhone, pPhoneVal, L"String", L"String");
        }
        delete pPhoneKey;
        JsonString* pUserStatusKey = new JsonString(L"userStatus");
        IJsonValue* pUserStatusVal = null;
        pJsonObject->GetValue(pUserStatusKey, pUserStatusVal);
        if(pUserStatusVal != null) {
            
            pUserStatus = new Integer();
            jsonToValue(pUserStatus, pUserStatusVal, L"Integer", L"Integer");
        }
        delete pUserStatusKey;
        
    }
}

SamiUser::SamiUser(String* json) {
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
SamiUser::asJson ()
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
SamiUser::asJsonObject() {
    JsonObject *pJsonObject = new JsonObject();
    pJsonObject->Construct();

    
    JsonString *pIdKey = new JsonString(L"id");
    pJsonObject->Add(pIdKey, toJson(getPId(), "Long", ""));

    
    JsonString *pUsernameKey = new JsonString(L"username");
    pJsonObject->Add(pUsernameKey, toJson(getPUsername(), "String", ""));

    
    JsonString *pFirstNameKey = new JsonString(L"firstName");
    pJsonObject->Add(pFirstNameKey, toJson(getPFirstName(), "String", ""));

    
    JsonString *pLastNameKey = new JsonString(L"lastName");
    pJsonObject->Add(pLastNameKey, toJson(getPLastName(), "String", ""));

    
    JsonString *pEmailKey = new JsonString(L"email");
    pJsonObject->Add(pEmailKey, toJson(getPEmail(), "String", ""));

    
    JsonString *pPasswordKey = new JsonString(L"password");
    pJsonObject->Add(pPasswordKey, toJson(getPPassword(), "String", ""));

    
    JsonString *pPhoneKey = new JsonString(L"phone");
    pJsonObject->Add(pPhoneKey, toJson(getPPhone(), "String", ""));

    
    JsonString *pUserStatusKey = new JsonString(L"userStatus");
    pJsonObject->Add(pUserStatusKey, toJson(getPUserStatus(), "Integer", ""));

    
    return pJsonObject;
}

Long*
SamiUser::getPId() {
    return pId;
}
void
SamiUser::setPId(Long* pId) {
    this->pId = pId;
}

String*
SamiUser::getPUsername() {
    return pUsername;
}
void
SamiUser::setPUsername(String* pUsername) {
    this->pUsername = pUsername;
}

String*
SamiUser::getPFirstName() {
    return pFirstName;
}
void
SamiUser::setPFirstName(String* pFirstName) {
    this->pFirstName = pFirstName;
}

String*
SamiUser::getPLastName() {
    return pLastName;
}
void
SamiUser::setPLastName(String* pLastName) {
    this->pLastName = pLastName;
}

String*
SamiUser::getPEmail() {
    return pEmail;
}
void
SamiUser::setPEmail(String* pEmail) {
    this->pEmail = pEmail;
}

String*
SamiUser::getPPassword() {
    return pPassword;
}
void
SamiUser::setPPassword(String* pPassword) {
    this->pPassword = pPassword;
}

String*
SamiUser::getPPhone() {
    return pPhone;
}
void
SamiUser::setPPhone(String* pPhone) {
    this->pPhone = pPhone;
}

Integer*
SamiUser::getPUserStatus() {
    return pUserStatus;
}
void
SamiUser::setPUserStatus(Integer* pUserStatus) {
    this->pUserStatus = pUserStatus;
}



} /* namespace Swagger */

