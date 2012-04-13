/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen.config;

/**
 * class to translate reserved words to safe variable names
 * 
 * @author tony
 *
 */
public class DefaultReservedWordMapper implements ReservedWordMapper {
	@Override
	public String translate(String input) {
		//	does nothing
		return input;
	}

    @Override
    public String retranslate(String input) {
        //	does nothing
        return input;
    }

}
