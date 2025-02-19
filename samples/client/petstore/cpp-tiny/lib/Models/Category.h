
/*
 * Category.h
 *
 * A category for a pet
 */

#ifndef TINY_CPP_CLIENT_Category_H_
#define TINY_CPP_CLIENT_Category_H_


#include <string>
#include "bourne/json.hpp"
#include "Helpers.h"

namespace Tiny {


/*! \brief A category for a pet
 *
 *  \ingroup Models
 *
 */

class Category{
public:

    /*! \brief Constructor.
	 */
    Category();
    Category(std::string jsonString);


    /*! \brief Destructor.
	 */
    virtual ~Category();


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
	std::string getName();

	/*! \brief Set 
	 */
	void setName(std::string  name);


    private:
    long id{};
    std::string name{};
};
}

#endif /* TINY_CPP_CLIENT_Category_H_ */
