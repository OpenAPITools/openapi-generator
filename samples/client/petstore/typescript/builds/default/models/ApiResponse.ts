/*
	TODO: LICENSE INFO
*/

/**
* Describes the result of uploading an image resource
*/
export class ApiResponse {
    'code'?: number;
    'type'?: string;
    'message'?: string;

    static readonly discriminator: string | undefined = undefined;

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string, format: string}> = [
        {
            "name": "code",
            "baseName": "code",
            "type": "number",
            "format": "int32"
        },
        {
            "name": "type",
            "baseName": "type",
            "type": "string",
            "format": ""
        },
        {
            "name": "message",
            "baseName": "message",
            "type": "string",
            "format": ""
        }    ];

    static getAttributeTypeMap() {
        return ApiResponse.attributeTypeMap;
    }
    
    public constructor() {
    }
}

