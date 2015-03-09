#ifndef SamiError_H_
#define SamiError_H_

#include <FBase.h>

using namespace Tizen::Base;

namespace Swagger {

class SamiError {
public:
    SamiError();
    SamiError(int code, String* message);
    virtual ~SamiError();


    void init();

    void cleanup();

    int getCode();
    void setCode(int pCode);
    
    String* getMessage();
    void setMessage(String* pMessage);
    

private:
    int pCode;
    String* pMessage;
};

} /* namespace Swagger */

#endif /* SamiError_H_ */
