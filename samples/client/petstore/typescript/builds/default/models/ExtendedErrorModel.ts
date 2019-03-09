/*
	TODO: LICENSE INFO
*/
import { ErrorModel } from './ErrorModel';

export class ExtendedErrorModel extends ErrorModel {
    'rootCause': string;

    static readonly discriminator: string | undefined = undefined;

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string}> = [
        {
            "name": "rootCause",
            "baseName": "rootCause",
            "type": "string"
        }    ];

    static getAttributeTypeMap() {
        return super.getAttributeTypeMap().concat(ExtendedErrorModel.attributeTypeMap);
    }
    
    public constructor() {
        super();
    }
}

