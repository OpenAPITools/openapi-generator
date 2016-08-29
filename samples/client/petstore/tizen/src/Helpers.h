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

#ifndef _HELPERS_H_
#define _HELPERS_H_

#include <string>
#include <glib-object.h>
#include <json-glib/json-glib.h>

using namespace std;

JsonNode *
json_from_string (const char  *str,
				  GError **mygerror);

char *
json_to_string (JsonNode *node,
				gboolean  pretty);

JsonNode*
converttoJson(void* v, string type, string containerType);

void
jsonToValue(void* target, JsonNode* ptr, string type, string innerType);

void helper_func(JsonObject *object, const gchar* member_name, JsonNode *member_node,gpointer user_data);

string
stringify(void* ptr, string type);

bool isprimitive(string type);
#endif /* HELPERS_H_ */
