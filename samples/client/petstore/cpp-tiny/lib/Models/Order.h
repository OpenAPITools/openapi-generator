
/*
 * Order.h
 *
 * An order for a pets from the pet store
 */

#ifndef TINY_CPP_CLIENT_Order_H_
#define TINY_CPP_CLIENT_Order_H_


#include <string>
#include "bourne/json.hpp"
#include "Helpers.h"

namespace Tiny {


/*! \brief An order for a pets from the pet store
 *
 *  \ingroup Models
 *
 */

class Order{
public:

    /*! \brief Constructor.
	 */
    Order();
    Order(std::string jsonString);


    /*! \brief Destructor.
	 */
    virtual ~Order();


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
	long getPetId();

	/*! \brief Set 
	 */
	void setPetId(long  petId);
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
	bool isComplete();

	/*! \brief Set 
	 */
	void setComplete(bool  complete);


    private:
    long id{};
    long petId{};
    int quantity{};
    std::string shipDate{};
    std::string status{};
    bool complete{};
};
}

#endif /* TINY_CPP_CLIENT_Order_H_ */
