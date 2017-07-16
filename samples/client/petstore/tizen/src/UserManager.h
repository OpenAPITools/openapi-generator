#ifndef _UserManager_H_
#define _UserManager_H_

#include <string>
#include <cstring>
#include <list>
#include <glib.h>
#include "User.h"
#include <list>
#include "Error.h"

/** \defgroup Operations API Endpoints
 *  Classes containing all the functions for calling API endpoints
 *
 */

namespace Tizen{
namespace ArtikCloud {
/** \addtogroup User User
 * \ingroup Operations
 *  @{
 */
class UserManager {
public:
	UserManager();
	virtual ~UserManager();

/*! \brief Create user. *Synchronous*
 *
 * This can only be done by the logged in user.
 * \param body Created user object *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool createUserSync(char * accessToken,
	User body, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Create user. *Asynchronous*
 *
 * This can only be done by the logged in user.
 * \param body Created user object *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool createUserAsync(char * accessToken,
	User body, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Creates list of users with given input array. *Synchronous*
 *
 * 
 * \param body List of user object *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool createUsersWithArrayInputSync(char * accessToken,
	std::list<User> body, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Creates list of users with given input array. *Asynchronous*
 *
 * 
 * \param body List of user object *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool createUsersWithArrayInputAsync(char * accessToken,
	std::list<User> body, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Creates list of users with given input array. *Synchronous*
 *
 * 
 * \param body List of user object *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool createUsersWithListInputSync(char * accessToken,
	std::list<User> body, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Creates list of users with given input array. *Asynchronous*
 *
 * 
 * \param body List of user object *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool createUsersWithListInputAsync(char * accessToken,
	std::list<User> body, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Delete user. *Synchronous*
 *
 * This can only be done by the logged in user.
 * \param username The name that needs to be deleted *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool deleteUserSync(char * accessToken,
	std::string username, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Delete user. *Asynchronous*
 *
 * This can only be done by the logged in user.
 * \param username The name that needs to be deleted *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool deleteUserAsync(char * accessToken,
	std::string username, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Get user by user name. *Synchronous*
 *
 * 
 * \param username The name that needs to be fetched. Use user1 for testing.  *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool getUserByNameSync(char * accessToken,
	std::string username, 
	void(* handler)(User, Error, void* )
	, void* userData);

/*! \brief Get user by user name. *Asynchronous*
 *
 * 
 * \param username The name that needs to be fetched. Use user1 for testing.  *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool getUserByNameAsync(char * accessToken,
	std::string username, 
	void(* handler)(User, Error, void* )
	, void* userData);


/*! \brief Logs user into the system. *Synchronous*
 *
 * 
 * \param username The user name for login *Required*
 * \param password The password for login in clear text *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool loginUserSync(char * accessToken,
	std::string username, std::string password, 
	void(* handler)(std::string, Error, void* )
	, void* userData);

/*! \brief Logs user into the system. *Asynchronous*
 *
 * 
 * \param username The user name for login *Required*
 * \param password The password for login in clear text *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool loginUserAsync(char * accessToken,
	std::string username, std::string password, 
	void(* handler)(std::string, Error, void* )
	, void* userData);


/*! \brief Logs out current logged in user session. *Synchronous*
 *
 * 
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool logoutUserSync(char * accessToken,
	
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Logs out current logged in user session. *Asynchronous*
 *
 * 
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool logoutUserAsync(char * accessToken,
	
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Updated user. *Synchronous*
 *
 * This can only be done by the logged in user.
 * \param username name that need to be deleted *Required*
 * \param body Updated user object *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool updateUserSync(char * accessToken,
	std::string username, User body, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Updated user. *Asynchronous*
 *
 * This can only be done by the logged in user.
 * \param username name that need to be deleted *Required*
 * \param body Updated user object *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool updateUserAsync(char * accessToken,
	std::string username, User body, 
	
	void(* handler)(Error, void* ) , void* userData);



	static std::string getBasePath()
	{
		return "http://petstore.swagger.io/v2";
	}
};
/** @}*/

}
}
#endif /* UserManager_H_ */
