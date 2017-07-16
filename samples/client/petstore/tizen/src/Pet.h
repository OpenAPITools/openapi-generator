/*
 * Pet.h
 *
 * A pet for sale in the pet store
 */

#ifndef _Pet_H_
#define _Pet_H_


#include <string>
#include "Category.h"
#include "Tag.h"
#include <list>
#include "Object.h"

/** \defgroup Models Data Structures for API
 *  Classes containing all the Data Structures needed for calling/returned by API endpoints
 *
 */

namespace Tizen {
namespace ArtikCloud {


/*! \brief A pet for sale in the pet store
 *
 *  \ingroup Models
 *
 */

class Pet : public Object {
public:
	/*! \brief Constructor.
	 */
	Pet();
	Pet(char* str);

	/*! \brief Destructor.
	 */
	virtual ~Pet();

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
	Category getCategory();

	/*! \brief Set 
	 */
	void setCategory(Category  category);
	/*! \brief Get 
	 */
	std::string getName();

	/*! \brief Set 
	 */
	void setName(std::string  name);
	/*! \brief Get 
	 */
	std::list<std::string> getPhotoUrls();

	/*! \brief Set 
	 */
	void setPhotoUrls(std::list <std::string> photoUrls);
	/*! \brief Get 
	 */
	std::list<Tag> getTags();

	/*! \brief Set 
	 */
	void setTags(std::list <Tag> tags);
	/*! \brief Get pet status in the store
	 */
	std::string getStatus();

	/*! \brief Set pet status in the store
	 */
	void setStatus(std::string  status);

private:
	long long id;
	Category category;
	std::string name;
	std::list <std::string>photoUrls;
	std::list <Tag>tags;
	std::string status;
	void __init();
	void __cleanup();

};
}
}

#endif /* _Pet_H_ */
