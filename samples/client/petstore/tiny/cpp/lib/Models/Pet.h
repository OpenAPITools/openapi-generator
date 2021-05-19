
/*
 * Pet.h
 *
 * A pet for sale in the pet store
 */

#ifndef TINY_CPP_CLIENT_Pet_H_
#define TINY_CPP_CLIENT_Pet_H_


#include <string>
#include "bourne/json.hpp"
#include "Helpers.h"
#include "Category.h"
#include "Tag.h"
#include <list>

namespace Tiny {


/*! \brief A pet for sale in the pet store
 *
 *  \ingroup Models
 *
 */

class Pet{
public:

    /*! \brief Constructor.
	 */
    Pet();
    Pet(std::string jsonString);


    /*! \brief Destructor.
	 */
    virtual ~Pet();


    /*! \brief Retrieve a bourne JSON representation of this class.
	 */
    bourne::json toJson();


    /*! \brief Fills in members of this class from bourne JSON object representing it.
	 */
    void fromJson(std::string jsonObj);

	/*! \brief Get 
	 */
	long getId();

	/*! \brief Set 
	 */
	void setId(long  id);
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
    long id{};
    Category category;
    std::string name{};
    std::list<std::string> photoUrls;
    std::list<Tag> tags;
    std::string status{};
};
}

#endif /* TINY_CPP_CLIENT_Pet_H_ */
