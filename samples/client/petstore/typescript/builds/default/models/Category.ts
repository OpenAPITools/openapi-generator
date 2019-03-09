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

    private static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string}> = [
        {
            "name": "id",
            "baseName": "id",
            "type": "number"
        },
        {
            "name": "name",
            "baseName": "name",
            "type": "string"
        }    ];

    static getAttributeTypeMap() {
        return Category.attributeTypeMap;
    }
}

