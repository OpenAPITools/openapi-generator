/*
 * SamiUser.h
 * 
 * 
 */

#ifndef SamiUser_H_
#define SamiUser_H_

#include <FApp.h>
#include <FBase.h>
#include <FSystem.h>
#include <FWebJson.h>
#include "SamiHelpers.h"
#include "SamiObject.h"

using namespace Tizen::Web::Json;


using Tizen::Base::Long;
using Tizen::Base::String;
using Tizen::Base::Integer;


namespace Swagger {

class SamiUser: public SamiObject {
public:
    SamiUser();
    SamiUser(String* json);
    virtual ~SamiUser();

    void init();

    void cleanup();

    String asJson ();

    JsonObject* asJsonObject();

    void fromJsonObject(IJsonValue* json);

    SamiUser* fromJson(String* obj);

    
    Long* getPId();
    void setPId(Long* pId);
    
    String* getPUsername();
    void setPUsername(String* pUsername);
    
    String* getPFirstName();
    void setPFirstName(String* pFirstName);
    
    String* getPLastName();
    void setPLastName(String* pLastName);
    
    String* getPEmail();
    void setPEmail(String* pEmail);
    
    String* getPPassword();
    void setPPassword(String* pPassword);
    
    String* getPPhone();
    void setPPhone(String* pPhone);
    
    Integer* getPUserStatus();
    void setPUserStatus(Integer* pUserStatus);
    

private:
    Long* pId;
    String* pUsername;
    String* pFirstName;
    String* pLastName;
    String* pEmail;
    String* pPassword;
    String* pPhone;
    Integer* pUserStatus;
    
};

} /* namespace Swagger */

#endif /* SamiUser_H_ */
