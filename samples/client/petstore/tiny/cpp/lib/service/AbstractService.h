#ifndef _AbstractService_H_
#define _AbstractService_H_

#include "HTTPClient.h"
#include "Response.h"
namespace Tiny {

/**
*  Class
* Generated with openapi::tiny-cpp-client
*/
class AbstractService {
public:
HTTPClient http;
std::string basepath = "https://petstore3.swagger.io/api/v3"; // TODO: change to your url

void begin(std::string url);

// Certificate from file
const char* test_root_ca =
#include "../../root.cert"
;

}; // end class
}// namespace Tinyclient
#endif // _AbstractService_H_
