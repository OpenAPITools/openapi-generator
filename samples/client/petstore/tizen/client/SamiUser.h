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

    
    Long* getId();
    void setId(Long* pId);
    
    String* getUsername();
    void setUsername(String* pUsername);
    
    String* getFirstName();
    void setFirstName(String* pFirstName);
    
    String* getLastName();
    void setLastName(String* pLastName);
    
    String* getEmail();
    void setEmail(String* pEmail);
    
    String* getPassword();
    void setPassword(String* pPassword);
    
    String* getPhone();
    void setPhone(String* pPhone);
    
    Integer* getUserStatus();
    void setUserStatus(Integer* pUserStatus);
    

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
