#ifndef _PetManager_H_
#define _PetManager_H_

#include <string>
#include <cstring>
#include <list>
#include <glib.h>
#include "ApiResponse.h"
#include "Pet.h"
#include "Error.h"

/** \defgroup Operations API Endpoints
 *  Classes containing all the functions for calling API endpoints
 *
 */

namespace Tizen{
namespace ArtikCloud {
/** \addtogroup Pet Pet
 * \ingroup Operations
 *  @{
 */
class PetManager {
public:
	PetManager();
	virtual ~PetManager();

/*! \brief Add a new pet to the store. *Synchronous*
 *
 * 
 * \param body Pet object that needs to be added to the store *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool addPetSync(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Add a new pet to the store. *Asynchronous*
 *
 * 
 * \param body Pet object that needs to be added to the store *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool addPetAsync(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Deletes a pet. *Synchronous*
 *
 * 
 * \param petId Pet id to delete *Required*
 * \param apiKey 
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool deletePetSync(char * accessToken,
	long long petId, std::string apiKey, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Deletes a pet. *Asynchronous*
 *
 * 
 * \param petId Pet id to delete *Required*
 * \param apiKey 
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool deletePetAsync(char * accessToken,
	long long petId, std::string apiKey, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Finds Pets by status. *Synchronous*
 *
 * Multiple status values can be provided with comma separated strings
 * \param status Status values that need to be considered for filter *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool findPetsByStatusSync(char * accessToken,
	std::list<std::string> status, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData);

/*! \brief Finds Pets by status. *Asynchronous*
 *
 * Multiple status values can be provided with comma separated strings
 * \param status Status values that need to be considered for filter *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool findPetsByStatusAsync(char * accessToken,
	std::list<std::string> status, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData);


/*! \brief Finds Pets by tags. *Synchronous*
 *
 * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 * \param tags Tags to filter by *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool findPetsByTagsSync(char * accessToken,
	std::list<std::string> tags, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData);

/*! \brief Finds Pets by tags. *Asynchronous*
 *
 * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 * \param tags Tags to filter by *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool findPetsByTagsAsync(char * accessToken,
	std::list<std::string> tags, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData);


/*! \brief Find pet by ID. *Synchronous*
 *
 * Returns a single pet
 * \param petId ID of pet to return *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool getPetByIdSync(char * accessToken,
	long long petId, 
	void(* handler)(Pet, Error, void* )
	, void* userData);

/*! \brief Find pet by ID. *Asynchronous*
 *
 * Returns a single pet
 * \param petId ID of pet to return *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool getPetByIdAsync(char * accessToken,
	long long petId, 
	void(* handler)(Pet, Error, void* )
	, void* userData);


/*! \brief Update an existing pet. *Synchronous*
 *
 * 
 * \param body Pet object that needs to be added to the store *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool updatePetSync(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Update an existing pet. *Asynchronous*
 *
 * 
 * \param body Pet object that needs to be added to the store *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool updatePetAsync(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Updates a pet in the store with form data. *Synchronous*
 *
 * 
 * \param petId ID of pet that needs to be updated *Required*
 * \param name Updated name of the pet
 * \param status Updated status of the pet
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool updatePetWithFormSync(char * accessToken,
	long long petId, std::string name, std::string status, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Updates a pet in the store with form data. *Asynchronous*
 *
 * 
 * \param petId ID of pet that needs to be updated *Required*
 * \param name Updated name of the pet
 * \param status Updated status of the pet
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool updatePetWithFormAsync(char * accessToken,
	long long petId, std::string name, std::string status, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief uploads an image. *Synchronous*
 *
 * 
 * \param petId ID of pet to update *Required*
 * \param additionalMetadata Additional data to pass to server
 * \param file file to upload
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool uploadFileSync(char * accessToken,
	long long petId, std::string additionalMetadata, std::string file, 
	void(* handler)(ApiResponse, Error, void* )
	, void* userData);

/*! \brief uploads an image. *Asynchronous*
 *
 * 
 * \param petId ID of pet to update *Required*
 * \param additionalMetadata Additional data to pass to server
 * \param file file to upload
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool uploadFileAsync(char * accessToken,
	long long petId, std::string additionalMetadata, std::string file, 
	void(* handler)(ApiResponse, Error, void* )
	, void* userData);



	static std::string getBasePath()
	{
		return "http://petstore.swagger.io/v2";
	}
};
/** @}*/

}
}
#endif /* PetManager_H_ */
