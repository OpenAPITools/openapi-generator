#ifndef TINY_CPP_CLIENT_UserApi_H_
#define TINY_CPP_CLIENT_UserApi_H_


#include "Response.h"
#include "Arduino.h"
#include "Service.h"
#include "Helpers.h"
#include <list>

#include "User.h"
#include <list>

namespace Tiny {

/**
 *  Class 
 * Generated with openapi::tiny-cpp-client
 */

class UserApi : public Service {
public:
    UserApi() = default;

    virtual ~UserApi() = default;

    /**
    * Create user.
    *
    * This can only be done by the logged in user.
    * \param user Created user object *Required*
    */
    Response<
            String
        >
    createUser(
            
            User user
            
    );
    /**
    * Creates list of users with given input array.
    *
    * 
    * \param user List of user object *Required*
    */
    Response<
            String
        >
    createUsersWithArrayInput(
            std::list<User> user
            
            
    );
    /**
    * Creates list of users with given input array.
    *
    * 
    * \param user List of user object *Required*
    */
    Response<
            String
        >
    createUsersWithListInput(
            std::list<User> user
            
            
    );
    /**
    * Delete user.
    *
    * This can only be done by the logged in user.
    * \param username The name that needs to be deleted *Required*
    */
    Response<
            String
        >
    deleteUser(
            
            std::string username
            
    );
    /**
    * Get user by user name.
    *
    * 
    * \param username The name that needs to be fetched. Use user1 for testing. *Required*
    */
    Response<
                User
        >
    getUserByName(
            
            std::string username
            
    );
    /**
    * Logs user into the system.
    *
    * 
    * \param username The user name for login *Required*
    * \param password The password for login in clear text *Required*
    */
    Response<
                std::string
        >
    loginUser(
            
            std::string username
            , 
            
            std::string password
            
    );
    /**
    * Logs out current logged in user session.
    *
    * 
    */
    Response<
            String
        >
    logoutUser(
    );
    /**
    * Updated user.
    *
    * This can only be done by the logged in user.
    * \param username name that need to be deleted *Required*
    * \param user Updated user object *Required*
    */
    Response<
            String
        >
    updateUser(
            
            std::string username
            , 
            
            User user
            
    );
}; 

} 

#endif /* TINY_CPP_CLIENT_UserApi_H_ */