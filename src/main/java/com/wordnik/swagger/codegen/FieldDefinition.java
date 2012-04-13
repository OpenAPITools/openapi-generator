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

package com.wordnik.swagger.codegen;

import java.util.ArrayList;
import java.util.List;

public class FieldDefinition {
	private String returnType;
	private String name;
    private String originalName;
	private String initialization;
	private List<String> importDefinitions = new ArrayList<String>();
    private String collectionItemType;
    private String collectionItemName;

    private boolean hasListResponse;
    private boolean hasMapResponse;
    private boolean hasSetResponse;
    private boolean hasDateResponse;
    private boolean hasArrayResponse;

    private boolean hasPrimitiveType;

	public boolean isHasListResponse() {
		return hasListResponse;
	}

	public void setHasListResponse(boolean hasListResponse) {
		this.hasListResponse = hasListResponse;
	}

	public boolean isHasMapResponse() {
		return hasMapResponse;
	}

	public void setHasMapResponse(boolean hasMapResponse) {
		this.hasMapResponse = hasMapResponse;
	}

	public boolean isHasSetResponse() {
		return hasSetResponse;
	}

	public void setHasSetResponse(boolean hasSetResponse) {
		this.hasSetResponse = hasSetResponse;
	}

    public boolean isHasArrayResponse() {
        return hasArrayResponse;
    }

    public void setHasArrayResponse(boolean hasArrayResponse) {
        this.hasArrayResponse = hasArrayResponse;
    }

    public boolean isHasPrimitiveType() {
        return hasPrimitiveType;
    }

    public void setHasPrimitiveType(boolean hasPrimitiveType) {
        this.hasPrimitiveType = hasPrimitiveType;
    }

    public String getReturnType() {
		return returnType;
	}
	
	public void setReturnType(String returnType) {
		if(returnType.startsWith("List")){
		    hasListResponse = true;
		}else if(returnType.startsWith("Set")){
			hasSetResponse = true;
		}else if(returnType.startsWith("Map")){
			hasMapResponse = true;
		}else if(returnType.startsWith("Array")){
            hasArrayResponse = true;
        }
		this.returnType = returnType;
	}

    public String getOriginalName() {
        return originalName;
    }

    public void setOriginalName(String originalName) {
        this.originalName = originalName;
    }

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public String getInitialization() {
		return initialization;
	}
	
	public void setInitialization(String initialization) {
		this.initialization = initialization;
	}

	public List<String> getImportDefinitions() {
		return importDefinitions;
	}

    public String getNameForMethod() {
        return originalName.substring(0,1).toUpperCase() + originalName.substring(1);
    }

    public void setCollectionItemType(String collectionItemType) {
        this.collectionItemType = collectionItemType;
    }

    public String getCollectionItemType() {
        return collectionItemType;
    }

    public String getCollectionItemName() {
        return collectionItemName;
    }

    public void setCollectionItemName(String collectionItemName) {
        this.collectionItemName = collectionItemName;
    }

    public boolean isHasDateResponse() {
        return hasDateResponse;
    }

    public void setHasDateResponse(boolean hasDateResponse) {
        this.hasDateResponse = hasDateResponse;
    }
}
