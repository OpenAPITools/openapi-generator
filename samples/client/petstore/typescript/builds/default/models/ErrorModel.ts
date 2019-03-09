/*
	TODO: LICENSE INFO
*/

export class ErrorModel {
    'message': string;
    'code': number;

    static readonly discriminator: string | undefined = undefined;

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string}> = [
        {
            "name": "message",
            "baseName": "message",
            "type": "string"
        },
        {
            "name": "code",
            "baseName": "code",
            "type": "number"
        }    ];

    static getAttributeTypeMap() {
        return ErrorModel.attributeTypeMap;
    }
    
    public constructor() {
    }
}

