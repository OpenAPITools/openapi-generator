/*
 * IHttpBody.h
 * 
 * This is the interface for contents that can be sent to a remote HTTP server. 
 */

#ifndef IHttpBody_H_
#define IHttpBody_H_


#include <iostream>

namespace io {
namespace swagger {
namespace client {
namespace model {

class  IHttpBody
{
public:
    virtual ~IHttpBody() { }

    virtual void writeTo( std::ostream& stream ) = 0;
};

}
}
}
}

#endif /* IHttpBody_H_ */