#ifndef TINY_CPP_CLIENT_ABSTRACTSERVICE_H_
#define TINY_CPP_CLIENT_ABSTRACTSERVICE_H_

#include "HTTPClient.h"
#include "Response.h"
#include <list>
namespace Tiny {

/**
*  Class
* Generated with openapi::tiny-cpp-client
*/
class Service {
    public:
        HTTPClient http;
        std::string basepath = "https://petstore3.swagger.io/api/v3"; // TODO: change to your url
        std::string url = "";
        std::string payload = "";
        std::list<std::tuple<std::string, std::string>> queryParams;
        std::list<std::tuple<std::string, std::string>> formParams;

        int sendRequest(std::string url, const char * type, uint8_t * payload, size_t size);
        String getResponseBody();

        void addQueryParam(std::string key, std::string value);
        void addFormParam(std::string key, std::string value);
        void addHeader(std::string key, std::string value);

        // Go and comment out a certificate in root.cert, if you get an error here
        // Certificate from file
        const char* test_root_ca =
        #include "../../root.cert"
        ;

    private:
        void begin(std::string url);
        void prepareRequest();
        std::string encodeKeyValueTuple(std::list<std::tuple<std::string, std::string>> params);

}; // end class
}// namespace Tinyclient

#endif /* TINY_CPP_CLIENT_ABSTRACTSERVICE_H_ */
