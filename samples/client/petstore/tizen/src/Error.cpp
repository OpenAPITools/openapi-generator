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

#include "Error.h"

using namespace std;
using namespace Tizen::ArtikCloud;

Error::Error()
{
	init();
}

Error::Error(int code, string message)
{
	init();
	this->setCode(code);
	this->setMessage(message);
}

Error::~Error()
{
	this->cleanup();
}

void
Error::init()
{
	pCode = 0;
	pMessage = "";
}

void
Error::cleanup()
{

}

int
Error::getCode()
{
	return pCode;
}

void
Error::setCode(int pCode)
{
	this->pCode = pCode;
}

string
Error::getMessage()
{
	return pMessage;
}

void
Error::setMessage(string pMessage)
{
	this->pMessage = pMessage;
}
