#ifndef _StoreManager_H_
#define _StoreManager_H_

#include <string>
#include <cstring>
#include <list>
#include <glib.h>
#include "Order.h"
#include <map>
#include "Error.h"

/** \defgroup Operations API Endpoints
 *  Classes containing all the functions for calling API endpoints
 *
 */

namespace Tizen{
namespace ArtikCloud {
/** \addtogroup Store Store
 * \ingroup Operations
 *  @{
 */
class StoreManager {
public:
	StoreManager();
	virtual ~StoreManager();

/*! \brief Delete purchase order by ID. *Synchronous*
 *
 * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
 * \param orderId ID of the order that needs to be deleted *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool deleteOrderSync(char * accessToken,
	std::string orderId, 
	
	void(* handler)(Error, void* ) , void* userData);

/*! \brief Delete purchase order by ID. *Asynchronous*
 *
 * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
 * \param orderId ID of the order that needs to be deleted *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool deleteOrderAsync(char * accessToken,
	std::string orderId, 
	
	void(* handler)(Error, void* ) , void* userData);


/*! \brief Returns pet inventories by status. *Synchronous*
 *
 * Returns a map of status codes to quantities
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool getInventorySync(char * accessToken,
	
	void(* handler)(std::map<std::string,std::string>, Error, void* )
	, void* userData);

/*! \brief Returns pet inventories by status. *Asynchronous*
 *
 * Returns a map of status codes to quantities
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool getInventoryAsync(char * accessToken,
	
	void(* handler)(std::map<std::string,std::string>, Error, void* )
	, void* userData);


/*! \brief Find purchase order by ID. *Synchronous*
 *
 * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
 * \param orderId ID of pet that needs to be fetched *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool getOrderByIdSync(char * accessToken,
	long long orderId, 
	void(* handler)(Order, Error, void* )
	, void* userData);

/*! \brief Find purchase order by ID. *Asynchronous*
 *
 * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
 * \param orderId ID of pet that needs to be fetched *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool getOrderByIdAsync(char * accessToken,
	long long orderId, 
	void(* handler)(Order, Error, void* )
	, void* userData);


/*! \brief Place an order for a pet. *Synchronous*
 *
 * 
 * \param body order placed for purchasing the pet *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool placeOrderSync(char * accessToken,
	Order body, 
	void(* handler)(Order, Error, void* )
	, void* userData);

/*! \brief Place an order for a pet. *Asynchronous*
 *
 * 
 * \param body order placed for purchasing the pet *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool placeOrderAsync(char * accessToken,
	Order body, 
	void(* handler)(Order, Error, void* )
	, void* userData);



	static std::string getBasePath()
	{
		return "http://petstore.swagger.io/v2";
	}
};
/** @}*/

}
}
#endif /* StoreManager_H_ */
