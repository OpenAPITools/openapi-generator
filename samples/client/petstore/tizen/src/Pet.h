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
 * Pet.h
 *
 * 
 */

#ifndef _Pet_H_
#define _Pet_H_


#include <string>
#include "Category.h"
#include "Tag.h"
#include <list>
#include "Object.h"

namespace Tizen {
namespace ArtikCloud {


/*! \brief 
 *
 */

class Pet : public Object {
public:
	/*! \brief Constructor.
	 */
	Pet();
	Pet(char* str);
	
	/*! \brief Destructor.
	 */
	virtual ~Pet();

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
	long id;
	Category category;
	std::string name;
	std::list <std::string>photoUrls;
	std::list <Tag>tags;
	std::string status;
	void __init();
	void __cleanup();
	
};
}
}

#endif /* _Pet_H_ */
