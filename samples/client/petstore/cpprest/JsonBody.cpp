#include "JsonBody.h"

namespace io {
namespace swagger {
namespace client {
namespace model {

JsonBody::JsonBody( const web::json::value& json)
    : m_Json(json)
{ 
}

JsonBody::~JsonBody()
{
}

void JsonBody::writeTo( std::ostream& target )
{
    m_Json.serialize(target);
}

}
}
}
}
