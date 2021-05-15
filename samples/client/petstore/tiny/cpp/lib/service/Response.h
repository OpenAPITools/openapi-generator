#ifndef _Response_H_
#define _Response_H_
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
#endif
