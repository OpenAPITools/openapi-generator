/*
 * ApiResponse.h
 *
 * Describes the result of uploading an image resource
 */

#ifndef _ApiResponse_H_
#define _ApiResponse_H_


#include <string>
#include "Object.h"

/** \defgroup Models Data Structures for API
 *  Classes containing all the Data Structures needed for calling/returned by API endpoints
 *
 */

namespace Tizen {
namespace ArtikCloud {


/*! \brief Describes the result of uploading an image resource
 *
 *  \ingroup Models
 *
 */

class ApiResponse : public Object {
public:
	/*! \brief Constructor.
	 */
	ApiResponse();
	ApiResponse(char* str);

	/*! \brief Destructor.
	 */
	virtual ~ApiResponse();

	/*! \brief Retrieve a string JSON representation of this class.
	 */
	char* toJson();

	/*! \brief Fills in members of this class from JSON string representing it.
	 */
	void fromJson(char* jsonStr);

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
	int code;
	std::string type;
	std::string message;
	void __init();
	void __cleanup();

};
}
}

#endif /* _ApiResponse_H_ */
