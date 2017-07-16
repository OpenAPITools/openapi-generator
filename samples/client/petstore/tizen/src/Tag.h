/*
 * Tag.h
 *
 * A tag for a pet
 */

#ifndef _Tag_H_
#define _Tag_H_


#include <string>
#include "Object.h"

/** \defgroup Models Data Structures for API
 *  Classes containing all the Data Structures needed for calling/returned by API endpoints
 *
 */

namespace Tizen {
namespace ArtikCloud {


/*! \brief A tag for a pet
 *
 *  \ingroup Models
 *
 */

class Tag : public Object {
public:
	/*! \brief Constructor.
	 */
	Tag();
	Tag(char* str);

	/*! \brief Destructor.
	 */
	virtual ~Tag();

	/*! \brief Retrieve a string JSON representation of this class.
	 */
	char* toJson();

	/*! \brief Fills in members of this class from JSON string representing it.
	 */
	void fromJson(char* jsonStr);

	/*! \brief Get 
	 */
	long long getId();

	/*! \brief Set 
	 */
	void setId(long long  id);
	/*! \brief Get 
	 */
	std::string getName();

	/*! \brief Set 
	 */
	void setName(std::string  name);

private:
	long long id;
	std::string name;
	void __init();
	void __cleanup();

};
}
}

#endif /* _Tag_H_ */
