#ifndef TINY_CPP_CLIENT_ABSTRACTSERVICE_H_
#define TINY_CPP_CLIENT_ABSTRACTSERVICE_H_

#include "HTTPClient.h"
#include "Response.h"
#include <map>
namespace Tiny {

/**
*  Class
* Generated with openapi::tiny-cpp-client
*/
class AbstractService {
    public:
        HTTPClient http;
        std::string basepath = "https://petstore3.swagger.io/api/v3"; // TODO: change to your url
        std::map<std::string, std::string> queryParams;
        void begin(std::string url);

        int sendRequest(const char * type, Stream * stream, size_t size);

        void addQueryParam(std::string key, std::string value);
        void addHeader(std::string key, std::string value);

        // Go and comment out a certificate in root.cert, if you get an error here
        // Certificate from file
        const char* test_root_ca =
        #include "../../root.cert"
        ;

}; // end class
}// namespace Tinyclient

#endif /* TINY_CPP_CLIENT_ABSTRACTSERVICE_H_ */
