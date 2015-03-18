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

    
    Long* getpId();
    void setpId(Long* pId);
    
    String* getpUsername();
    void setpUsername(String* pUsername);
    
    String* getpFirstName();
    void setpFirstName(String* pFirstName);
    
    String* getpLastName();
    void setpLastName(String* pLastName);
    
    String* getpEmail();
    void setpEmail(String* pEmail);
    
    String* getpPassword();
    void setpPassword(String* pPassword);
    
    String* getpPhone();
    void setpPhone(String* pPhone);
    
    Integer* getpUserStatus();
    void setpUserStatus(Integer* pUserStatus);
    

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
