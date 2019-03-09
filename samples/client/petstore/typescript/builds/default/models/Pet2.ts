/*
	TODO: LICENSE INFO
*/

export class Pet2 {
    'name': string;
    'petType': string;

    static readonly discriminator: string | undefined = "petType";

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string}> = [
        {
            "name": "name",
            "baseName": "name",
            "type": "string"
        },
        {
            "name": "petType",
            "baseName": "petType",
            "type": "string"
        }    ];

    static getAttributeTypeMap() {
        return Pet2.attributeTypeMap;
    }
    
    public constructor() {
        this.petType = "Pet2";
    }
}

