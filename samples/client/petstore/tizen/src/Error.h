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

#ifndef _Error_H_
#define _Error_H_
#include <string>

namespace Tizen{
namespace ArtikCloud {

class Error {
public:
	Error();
	Error(int code, std::string message);
	virtual ~Error();


	void init();

	void cleanup();

	int getCode();
	void setCode(int pCode);

	std::string getMessage();
	void setMessage(std::string pMessage);


private:
	int pCode;
	std::string pMessage;
};

}
}
#endif /* Error_H_ */
