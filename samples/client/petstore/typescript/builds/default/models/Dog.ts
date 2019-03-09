/*
	TODO: LICENSE INFO
*/
import { Pet2 } from './Pet2';

/**
* A representation of a dog
*/
export class Dog extends Pet2 {
    /**
    * the size of the pack the dog is from
    */
    'packSize': number;

    static readonly discriminator: string | undefined = undefined;

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string}> = [
        {
            "name": "packSize",
            "baseName": "packSize",
            "type": "number"
        }    ];

    static getAttributeTypeMap() {
        return super.getAttributeTypeMap().concat(Dog.attributeTypeMap);
    }
    
    public constructor() {
        super();
        this.petType = "Dog";
    }
}

