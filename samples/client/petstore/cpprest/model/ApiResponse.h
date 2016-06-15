/*
 * ApiResponse.h
 * 
 * 
 */

#ifndef ApiResponse_H_
#define ApiResponse_H_


#include "ModelBase.h"

#include <cpprest/details/basic_types.h>

namespace io {
namespace swagger {
namespace client {
namespace model {

/// <summary>
/// 
/// </summary>
class  ApiResponse
	: public ModelBase
{
public:
    ApiResponse();
    virtual ~ApiResponse();

	/////////////////////////////////////////////
	/// ModelBase overrides
	
    void validate() override;

    web::json::value toJson() const override;
    void fromJson(web::json::value& json) override;

    void toMultipart(std::shared_ptr<MultipartFormData> multipart, const utility::string_t& namePrefix) const override;
    void fromMultiPart(std::shared_ptr<MultipartFormData> multipart, const utility::string_t& namePrefix) override;
    
 	/////////////////////////////////////////////
	/// ApiResponse members
	   
    /// <summary>
    /// 
    /// </summary>
    int32_t getCode() const;
    void setCode(int32_t value);
    bool codeIsSet() const;
    void unsetCode();
    /// <summary>
    /// 
    /// </summary>
    utility::string_t getType() const;
    void setType(utility::string_t value);
    bool typeIsSet() const;
    void unsetType();
    /// <summary>
    /// 
    /// </summary>
    utility::string_t getMessage() const;
    void setMessage(utility::string_t value);
    bool messageIsSet() const;
    void unsetMessage();
    
protected:
    int32_t m_Code;
    bool m_CodeIsSet;
utility::string_t m_Type;
    bool m_TypeIsSet;
utility::string_t m_Message;
    bool m_MessageIsSet;
};

}
}
}
}

#endif /* ApiResponse_H_ */
