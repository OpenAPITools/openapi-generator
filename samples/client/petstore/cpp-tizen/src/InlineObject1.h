/*
 * Inline_object_1.h
 *
 * 
 */

#ifndef _Inline_object_1_H_
#define _Inline_object_1_H_


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

class Inline_object_1 : public Object {
public:
	/*! \brief Constructor.
	 */
	Inline_object_1();
	Inline_object_1(char* str);

	/*! \brief Destructor.
	 */
	virtual ~Inline_object_1();

	/*! \brief Retrieve a string JSON representation of this class.
	 */
	char* toJson();

	/*! \brief Fills in members of this class from JSON string representing it.
	 */
	void fromJson(char* jsonStr);

	/*! \brief Get Additional data to pass to server
	 */
	std::string getAdditionalMetadata();

	/*! \brief Set Additional data to pass to server
	 */
	void setAdditionalMetadata(std::string  additionalMetadata);
	/*! \brief Get file to upload
	 */
	std::string getFile();

	/*! \brief Set file to upload
	 */
	void setFile(std::string  file);

private:
	std::string additionalMetadata;
	std::string file;
	void __init();
	void __cleanup();

};
}
}

#endif /* _Inline_object_1_H_ */
