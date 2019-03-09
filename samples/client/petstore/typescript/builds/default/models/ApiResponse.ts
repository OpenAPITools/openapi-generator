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

    private static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string}> = [
        {
            "name": "code",
            "baseName": "code",
            "type": "number"
        },
        {
            "name": "type",
            "baseName": "type",
            "type": "string"
        },
        {
            "name": "message",
            "baseName": "message",
            "type": "string"
        }    ];

    static getAttributeTypeMap() {
        return ApiResponse.attributeTypeMap;
    }
}

