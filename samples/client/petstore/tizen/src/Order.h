/*
 * Copyright (c) 2016 Samsung Electronics Co., Ltd All Rights Reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Order.h
 *
 * 
 */

#ifndef _Order_H_
#define _Order_H_


#include <string>
#include "Object.h"

namespace Tizen {
namespace ArtikCloud {


/*! \brief 
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
	bool getComplete();

	/*! \brief Set 
	 */
	void setComplete(bool  complete);

private:
	long id;
	long petId;
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
