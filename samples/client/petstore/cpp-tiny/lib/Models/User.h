
/*
 * User.h
 *
 * A User who is purchasing from the pet store
 */

#ifndef TINY_CPP_CLIENT_User_H_
#define TINY_CPP_CLIENT_User_H_


#include <string>
#include "bourne/json.hpp"
#include "Helpers.h"

namespace Tiny {


/*! \brief A User who is purchasing from the pet store
 *
 *  \ingroup Models
 *
 */

class User{
public:

    /*! \brief Constructor.
	 */
    User();
    User(std::string jsonString);


    /*! \brief Destructor.
	 */
    virtual ~User();


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
	std::string getUsername();

	/*! \brief Set 
	 */
	void setUsername(std::string  username);
	/*! \brief Get 
	 */
	std::string getFirstName();

	/*! \brief Set 
	 */
	void setFirstName(std::string  firstName);
	/*! \brief Get 
	 */
	std::string getLastName();

	/*! \brief Set 
	 */
	void setLastName(std::string  lastName);
	/*! \brief Get 
	 */
	std::string getEmail();

	/*! \brief Set 
	 */
	void setEmail(std::string  email);
	/*! \brief Get 
	 */
	std::string getPassword();

	/*! \brief Set 
	 */
	void setPassword(std::string  password);
	/*! \brief Get 
	 */
	std::string getPhone();

	/*! \brief Set 
	 */
	void setPhone(std::string  phone);
	/*! \brief Get User Status
	 */
	int getUserStatus();

	/*! \brief Set User Status
	 */
	void setUserStatus(int  userStatus);


    private:
    long id{};
    std::string username{};
    std::string firstName{};
    std::string lastName{};
    std::string email{};
    std::string password{};
    std::string phone{};
    int userStatus{};
};
}

#endif /* TINY_CPP_CLIENT_User_H_ */
