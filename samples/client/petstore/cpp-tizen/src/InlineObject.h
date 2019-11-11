/*
 * Inline_object.h
 *
 * 
 */

#ifndef _Inline_object_H_
#define _Inline_object_H_


#include <string>
#include "Object.h"

/** \defgroup Models Data Structures for API
 *  Classes containing all the Data Structures needed for calling/returned by API endpoints
 *
 */

namespace Tizen {
namespace ArtikCloud {


/*! \brief 
 *
 *  \ingroup Models
 *
 */

class Inline_object : public Object {
public:
	/*! \brief Constructor.
	 */
	Inline_object();
	Inline_object(char* str);

	/*! \brief Destructor.
	 */
	virtual ~Inline_object();

	/*! \brief Retrieve a string JSON representation of this class.
	 */
	char* toJson();

	/*! \brief Fills in members of this class from JSON string representing it.
	 */
	void fromJson(char* jsonStr);

	/*! \brief Get Updated name of the pet
	 */
	std::string getName();

	/*! \brief Set Updated name of the pet
	 */
	void setName(std::string  name);
	/*! \brief Get Updated status of the pet
	 */
	std::string getStatus();

	/*! \brief Set Updated status of the pet
	 */
	void setStatus(std::string  status);

private:
	std::string name;
	std::string status;
	void __init();
	void __cleanup();

};
}
}

#endif /* _Inline_object_H_ */
