/*
 * Order.h
 *
 * An order for a pets from the pet store
 */

#ifndef _Order_H_
#define _Order_H_


#include <string>
#include "Object.h"

/** \defgroup Models Data Structures for API
 *  Classes containing all the Data Structures needed for calling/returned by API endpoints
 *
 */

namespace Tizen {
namespace ArtikCloud {


/*! \brief An order for a pets from the pet store
 *
 *  \ingroup Models
 *
 */

class Order : public Object {
public:
	/*! \brief Constructor.
	 */
	Order();
	Order(char* str);

	/*! \brief Destructor.
	 */
	virtual ~Order();

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
	long long getPetId();

	/*! \brief Set 
	 */
	void setPetId(long long  petId);
	/*! \brief Get 
	 */
	int getQuantity();

	/*! \brief Set 
	 */
	void setQuantity(int  quantity);
	/*! \brief Get 
	 */
	std::string getShipDate();

	/*! \brief Set 
	 */
	void setShipDate(std::string  shipDate);
	/*! \brief Get Order Status
	 */
	std::string getStatus();

	/*! \brief Set Order Status
	 */
	void setStatus(std::string  status);
	/*! \brief Get 
	 */
	bool getComplete();

	/*! \brief Set 
	 */
	void setComplete(bool  complete);

private:
	long long id;
	long long petId;
	int quantity;
	std::string shipDate;
	std::string status;
	bool complete;
	void __init();
	void __cleanup();

};
}
}

#endif /* _Order_H_ */
