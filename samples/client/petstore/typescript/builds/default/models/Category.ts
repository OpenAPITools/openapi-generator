/*
	TODO: LICENSE INFO
*/

/**
* A category for a pet
*/
export class Category {
    'id'?: number;
    'name'?: string;

    static readonly discriminator: string | undefined = undefined;

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string, format: string}> = [
        {
            "name": "id",
            "baseName": "id",
            "type": "number",
            "format": "int64"
        },
        {
            "name": "name",
            "baseName": "name",
            "type": "string",
            "format": ""
        }    ];

    static getAttributeTypeMap() {
        return Category.attributeTypeMap;
    }
    
    public constructor() {
    }
}

