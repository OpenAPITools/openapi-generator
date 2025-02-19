
/*
 * ApiResponse.h
 *
 * Describes the result of uploading an image resource
 */

#ifndef TINY_CPP_CLIENT_ApiResponse_H_
#define TINY_CPP_CLIENT_ApiResponse_H_


#include <string>
#include "bourne/json.hpp"
#include "Helpers.h"

namespace Tiny {


/*! \brief Describes the result of uploading an image resource
 *
 *  \ingroup Models
 *
 */

class ApiResponse{
public:

    /*! \brief Constructor.
	 */
    ApiResponse();
    ApiResponse(std::string jsonString);


    /*! \brief Destructor.
	 */
    virtual ~ApiResponse();


    /*! \brief Retrieve a bourne JSON representation of this class.
	 */
    bourne::json toJson();


    /*! \brief Fills in members of this class from bourne JSON object representing it.
	 */
    void fromJson(std::string jsonObj);

	/*! \brief Get 
	 */
	int getCode();

	/*! \brief Set 
	 */
	void setCode(int  code);
	/*! \brief Get 
	 */
	std::string getType();

	/*! \brief Set 
	 */
	void setType(std::string  type);
	/*! \brief Get 
	 */
	std::string getMessage();

	/*! \brief Set 
	 */
	void setMessage(std::string  message);


    private:
    int code{};
    std::string type{};
    std::string message{};
};
}

#endif /* TINY_CPP_CLIENT_ApiResponse_H_ */
