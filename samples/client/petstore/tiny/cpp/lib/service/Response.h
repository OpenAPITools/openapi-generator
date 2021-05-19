#ifndef TINY_CPP_CLIENT_RESPONSE_H_
#define TINY_CPP_CLIENT_RESPONSE_H_
#include <string>

namespace Tiny {

/**
*  Class
* Generated with openapi::tiny-cpp-client
*/
template <typename T = std::string>
    class Response {
    public:

    Response(T _obj, int _code){
        obj = _obj;
        code = _code;
    }

    int code;
    T obj;
    };
    } // namespace Tinyclient
    
#endif /* TINY_CPP_CLIENT_RESPONSE_H_ */
