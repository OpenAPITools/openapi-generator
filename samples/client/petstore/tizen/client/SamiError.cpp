#include "SamiError.h"

using namespace Tizen::Base;
using namespace Tizen::System;

namespace Swagger {

SamiError::SamiError() {
    init();
}

SamiError::SamiError(int code, String* message) {
    init();
    this->setCode(code);
    this->setMessage(message);
}

SamiError::~SamiError() {
    this->cleanup();
}

void
SamiError::init() {
    pCode = 0;
    pMessage = null;
}

void
SamiError::cleanup() {
    if(pMessage != null) {
        delete pMessage;
        pMessage = null;
    }
}

int
SamiError::getCode() {
    return pCode;
}
void
SamiError::setCode(int pCode) {
    this->pCode = pCode;
}

String*
SamiError::getMessage() {
    return pMessage;
}
void
SamiError::setMessage(String* pMessage) {
    this->pMessage = pMessage;
}

} /* namespace Swagger */

