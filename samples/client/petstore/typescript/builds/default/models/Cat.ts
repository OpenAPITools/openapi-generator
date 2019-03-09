/*
	TODO: LICENSE INFO
*/
import { Pet2 } from './Pet2';

/**
* A representation of a cat
*/
export class Cat extends Pet2 {
    /**
    * The measured skill for hunting
    */
    'huntingSkill': CatHuntingSkillEnum;

    static readonly discriminator: string | undefined = undefined;

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string}> = [
        {
            "name": "huntingSkill",
            "baseName": "huntingSkill",
            "type": "CatHuntingSkillEnum"
        }    ];

    static getAttributeTypeMap() {
        return super.getAttributeTypeMap().concat(Cat.attributeTypeMap);
    }
    
    public constructor() {
        super();
        this.petType = "Cat";
    }
}


export type CatHuntingSkillEnum = "clueless" | "lazy" | "adventurous" | "aggressive" ;

