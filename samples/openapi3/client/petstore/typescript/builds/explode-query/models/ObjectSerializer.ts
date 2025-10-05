export * from '../models/AdditionalPropertiesClass';
export * from '../models/AllOfWithSingleRef';
export * from '../models/Animal';
export * from '../models/ApiResponse';
export * from '../models/ArrayOfArrayOfNumberOnly';
export * from '../models/ArrayOfNumberOnly';
export * from '../models/ArrayTest';
export * from '../models/Capitalization';
export * from '../models/Cat';
export * from '../models/Category';
export * from '../models/ClassModel';
export * from '../models/Client';
export * from '../models/DeprecatedObject';
export * from '../models/Dog';
export * from '../models/EnumArrays';
export * from '../models/EnumClass';
export * from '../models/EnumTest';
export * from '../models/FakeBigDecimalMap200Response';
export * from '../models/FileSchemaTestClass';
export * from '../models/Foo';
export * from '../models/FooGetDefaultResponse';
export * from '../models/FormatTest';
export * from '../models/HasOnlyReadOnly';
export * from '../models/HealthCheckResult';
export * from '../models/List';
export * from '../models/MapTest';
export * from '../models/MixedPropertiesAndAdditionalPropertiesClass';
export * from '../models/Model200Response';
export * from '../models/ModelFile';
export * from '../models/Name';
export * from '../models/NullableClass';
export * from '../models/NumberOnly';
export * from '../models/ObjectWithDeprecatedFields';
export * from '../models/Order';
export * from '../models/OuterComposite';
export * from '../models/OuterEnum';
export * from '../models/OuterEnumDefaultValue';
export * from '../models/OuterEnumInteger';
export * from '../models/OuterEnumIntegerDefaultValue';
export * from '../models/OuterObjectWithEnumProperty';
export * from '../models/Pet';
export * from '../models/ReadOnlyFirst';
export * from '../models/Return';
export * from '../models/SingleRefType';
export * from '../models/SpecialModelName';
export * from '../models/Tag';
export * from '../models/User';

import { AdditionalPropertiesClass } from '../models/AdditionalPropertiesClass';
import { AllOfWithSingleRef   } from '../models/AllOfWithSingleRef';
import { Animal } from '../models/Animal';
import { ApiResponse } from '../models/ApiResponse';
import { ArrayOfArrayOfNumberOnly } from '../models/ArrayOfArrayOfNumberOnly';
import { ArrayOfNumberOnly } from '../models/ArrayOfNumberOnly';
import { ArrayTest } from '../models/ArrayTest';
import { Capitalization } from '../models/Capitalization';
import { Cat } from '../models/Cat';
import { Category } from '../models/Category';
import { ClassModel } from '../models/ClassModel';
import { Client } from '../models/Client';
import { DeprecatedObject } from '../models/DeprecatedObject';
import { Dog } from '../models/Dog';
import { EnumArrays, EnumArraysJustSymbolEnum  , EnumArraysArrayEnumEnum   } from '../models/EnumArrays';
import { EnumClass } from '../models/EnumClass';
import { EnumTest, EnumTestEnumStringEnum  , EnumTestEnumStringRequiredEnum  , EnumTestEnumIntegerEnum  , EnumTestEnumNumberEnum       } from '../models/EnumTest';
import { FakeBigDecimalMap200Response } from '../models/FakeBigDecimalMap200Response';
import { FileSchemaTestClass } from '../models/FileSchemaTestClass';
import { Foo } from '../models/Foo';
import { FooGetDefaultResponse } from '../models/FooGetDefaultResponse';
import { FormatTest } from '../models/FormatTest';
import { HasOnlyReadOnly } from '../models/HasOnlyReadOnly';
import { HealthCheckResult } from '../models/HealthCheckResult';
import { List } from '../models/List';
import { MapTest , MapTestMapOfEnumStringEnum     } from '../models/MapTest';
import { MixedPropertiesAndAdditionalPropertiesClass } from '../models/MixedPropertiesAndAdditionalPropertiesClass';
import { Model200Response } from '../models/Model200Response';
import { ModelFile } from '../models/ModelFile';
import { Name } from '../models/Name';
import { NullableClass } from '../models/NullableClass';
import { NumberOnly } from '../models/NumberOnly';
import { ObjectWithDeprecatedFields } from '../models/ObjectWithDeprecatedFields';
import { Order    , OrderStatusEnum    } from '../models/Order';
import { OuterComposite } from '../models/OuterComposite';
import { OuterEnum } from '../models/OuterEnum';
import { OuterEnumDefaultValue } from '../models/OuterEnumDefaultValue';
import { OuterEnumInteger } from '../models/OuterEnumInteger';
import { OuterEnumIntegerDefaultValue } from '../models/OuterEnumIntegerDefaultValue';
import { OuterObjectWithEnumProperty  } from '../models/OuterObjectWithEnumProperty';
import { Pet     , PetStatusEnum   } from '../models/Pet';
import { ReadOnlyFirst } from '../models/ReadOnlyFirst';
import { Return } from '../models/Return';
import { SingleRefType } from '../models/SingleRefType';
import { SpecialModelName } from '../models/SpecialModelName';
import { Tag } from '../models/Tag';
import { User } from '../models/User';

/* tslint:disable:no-unused-variable */
let primitives = [
                    "string",
                    "boolean",
                    "double",
                    "integer",
                    "long",
                    "float",
                    "number",
                    "any"
                 ];

let enumsMap: Set<string> = new Set<string>([
    "EnumArraysJustSymbolEnum",
    "EnumArraysArrayEnumEnum",
    "EnumClass",
    "EnumTestEnumStringEnum",
    "EnumTestEnumStringRequiredEnum",
    "EnumTestEnumIntegerEnum",
    "EnumTestEnumNumberEnum",
    "MapTestMapOfEnumStringEnum",
    "OrderStatusEnum",
    "OuterEnum",
    "OuterEnumDefaultValue",
    "OuterEnumInteger",
    "OuterEnumIntegerDefaultValue",
    "PetStatusEnum",
    "SingleRefType",
]);

let typeMap: {[index: string]: any} = {
    "AdditionalPropertiesClass": AdditionalPropertiesClass,
    "AllOfWithSingleRef": AllOfWithSingleRef,
    "Animal": Animal,
    "ApiResponse": ApiResponse,
    "ArrayOfArrayOfNumberOnly": ArrayOfArrayOfNumberOnly,
    "ArrayOfNumberOnly": ArrayOfNumberOnly,
    "ArrayTest": ArrayTest,
    "Capitalization": Capitalization,
    "Cat": Cat,
    "Category": Category,
    "ClassModel": ClassModel,
    "Client": Client,
    "DeprecatedObject": DeprecatedObject,
    "Dog": Dog,
    "EnumArrays": EnumArrays,
    "EnumTest": EnumTest,
    "FakeBigDecimalMap200Response": FakeBigDecimalMap200Response,
    "FileSchemaTestClass": FileSchemaTestClass,
    "Foo": Foo,
    "FooGetDefaultResponse": FooGetDefaultResponse,
    "FormatTest": FormatTest,
    "HasOnlyReadOnly": HasOnlyReadOnly,
    "HealthCheckResult": HealthCheckResult,
    "List": List,
    "MapTest": MapTest,
    "MixedPropertiesAndAdditionalPropertiesClass": MixedPropertiesAndAdditionalPropertiesClass,
    "Model200Response": Model200Response,
    "ModelFile": ModelFile,
    "Name": Name,
    "NullableClass": NullableClass,
    "NumberOnly": NumberOnly,
    "ObjectWithDeprecatedFields": ObjectWithDeprecatedFields,
    "Order": Order,
    "OuterComposite": OuterComposite,
    "OuterObjectWithEnumProperty": OuterObjectWithEnumProperty,
    "Pet": Pet,
    "ReadOnlyFirst": ReadOnlyFirst,
    "Return": Return,
    "SpecialModelName": SpecialModelName,
    "Tag": Tag,
    "User": User,
}

type MimeTypeDescriptor = {
    type: string;
    subtype: string;
    subtypeTokens: string[];
};

/**
 * Every mime-type consists of a type, subtype, and optional parameters.
 * The subtype can be composite, including information about the content format.
 * For example: `application/json-patch+json`, `application/merge-patch+json`.
 *
 * This helper transforms a string mime-type into an internal representation.
 * This simplifies the implementation of predicates that in turn define common rules for parsing or stringifying
 * the payload.
 */
const parseMimeType = (mimeType: string): MimeTypeDescriptor => {
    const [type = '', subtype = ''] = mimeType.split('/');
    return {
        type,
        subtype,
        subtypeTokens: subtype.split('+'),
    };
};

type MimeTypePredicate = (mimeType: string) => boolean;

// This factory creates a predicate function that checks a string mime-type against defined rules.
const mimeTypePredicateFactory = (predicate: (descriptor: MimeTypeDescriptor) => boolean): MimeTypePredicate => (mimeType) => predicate(parseMimeType(mimeType));

// Use this factory when you need to define a simple predicate based only on type and, if applicable, subtype.
const mimeTypeSimplePredicateFactory = (type: string, subtype?: string): MimeTypePredicate => mimeTypePredicateFactory((descriptor) => {
    if (descriptor.type !== type) return false;
    if (subtype != null && descriptor.subtype !== subtype) return false;
    return true;
});

// Creating a set of named predicates that will help us determine how to handle different mime-types
const isTextLikeMimeType = mimeTypeSimplePredicateFactory('text');
const isJsonMimeType = mimeTypeSimplePredicateFactory('application', 'json');
const isJsonLikeMimeType = mimeTypePredicateFactory((descriptor) => descriptor.type === 'application' && descriptor.subtypeTokens.some((item) => item === 'json'));
const isOctetStreamMimeType = mimeTypeSimplePredicateFactory('application', 'octet-stream');
const isFormUrlencodedMimeType = mimeTypeSimplePredicateFactory('application', 'x-www-form-urlencoded');

// Defining a list of mime-types in the order of prioritization for handling.
const supportedMimeTypePredicatesWithPriority: MimeTypePredicate[] = [
    isJsonMimeType,
    isJsonLikeMimeType,
    isTextLikeMimeType,
    isOctetStreamMimeType,
    isFormUrlencodedMimeType,
];

const nullableSuffix = " | null";
const optionalSuffix = " | undefined";
const arrayPrefix = "Array<";
const arraySuffix = ">";
const mapPrefix = "{ [key: string]: ";
const mapSuffix = "; }";

export class ObjectSerializer {
    public static findCorrectType(data: any, expectedType: string) {
        if (data == undefined) {
            return expectedType;
        } else if (primitives.indexOf(expectedType.toLowerCase()) !== -1) {
            return expectedType;
        } else if (expectedType === "Date") {
            return expectedType;
        } else {
            if (enumsMap.has(expectedType)) {
                return expectedType;
            }

            if (!typeMap[expectedType]) {
                return expectedType; // w/e we don't know the type
            }

            // Check the discriminator
            let discriminatorProperty = typeMap[expectedType].discriminator;
            if (discriminatorProperty == null) {
                return expectedType; // the type does not have a discriminator. use it.
            } else {
                if (data[discriminatorProperty]) {
                    var discriminatorType = data[discriminatorProperty];
                    let mapping = typeMap[expectedType].mapping;
                    if (mapping != undefined && mapping[discriminatorType]) {
                        return mapping[discriminatorType]; // use the type given in the discriminator
                    } else if(typeMap[discriminatorType]) {
                        return discriminatorType;
                    } else {
                        return expectedType; // discriminator did not map to a type
                    }
                } else {
                    return expectedType; // discriminator was not present (or an empty string)
                }
            }
        }
    }

    public static serialize(data: any, type: string, format: string): any {
        if (data == undefined) {
            return data;
        } else if (primitives.indexOf(type.toLowerCase()) !== -1) {
            return data;
        } else if (type.endsWith(nullableSuffix)) {
            let subType: string = type.slice(0, -nullableSuffix.length); // Type | null => Type
            return ObjectSerializer.serialize(data, subType, format);
        } else if (type.endsWith(optionalSuffix)) {
            let subType: string = type.slice(0, -optionalSuffix.length); // Type | undefined => Type
            return ObjectSerializer.serialize(data, subType, format);
        } else if (type.startsWith(arrayPrefix)) {
            let subType: string = type.slice(arrayPrefix.length, -arraySuffix.length); // Array<Type> => Type
            let transformedData: any[] = [];
            for (let date of data) {
                transformedData.push(ObjectSerializer.serialize(date, subType, format));
            }
            return transformedData;
        } else if (type.startsWith(mapPrefix)) {
            let subType: string = type.slice(mapPrefix.length, -mapSuffix.length); // { [key: string]: Type; } => Type
            let transformedData: { [key: string]: any } = {};
            for (let key in data) {
                transformedData[key] = ObjectSerializer.serialize(
                    data[key],
                    subType,
                    format,
                );
            }
            return transformedData;
        } else if (type === "Date") {
            if (!(data instanceof Date)) {
                return data;
            }
            if (format == "date") {
                let month = data.getMonth()+1
                let monthStr = month < 10 ? "0" + month.toString() : month.toString()
                let day = data.getDate();
                let dayStr = day < 10 ? "0" + day.toString() : day.toString();

                return data.getFullYear() + "-" + monthStr + "-" + dayStr;
            } else {
                return data.toISOString();
            }
        } else {
            if (enumsMap.has(type)) {
                return data;
            }
            if (!typeMap[type]) { // in case we dont know the type
                return data;
            }

            // Get the actual type of this object
            type = this.findCorrectType(data, type);

            // get the map for the correct type.
            let attributeTypes = typeMap[type].getAttributeTypeMap();
            let instance: {[index: string]: any} = {};
            for (let attributeType of attributeTypes) {
                instance[attributeType.baseName] = ObjectSerializer.serialize(data[attributeType.name], attributeType.type, attributeType.format);
            }
            return instance;
        }
    }

    public static deserialize(data: any, type: string, format: string): any {
        // polymorphism may change the actual type.
        type = ObjectSerializer.findCorrectType(data, type);
        if (data == undefined) {
            return data;
        } else if (primitives.indexOf(type.toLowerCase()) !== -1) {
            return data;
        } else if (type.endsWith(nullableSuffix)) {
            let subType: string = type.slice(0, -nullableSuffix.length); // Type | null => Type
            return ObjectSerializer.deserialize(data, subType, format);
        } else if (type.endsWith(optionalSuffix)) {
            let subType: string = type.slice(0, -optionalSuffix.length); // Type | undefined => Type
            return ObjectSerializer.deserialize(data, subType, format);
        } else if (type.startsWith(arrayPrefix)) {
            let subType: string = type.slice(arrayPrefix.length, -arraySuffix.length); // Array<Type> => Type
            let transformedData: any[] = [];
            for (let date of data) {
                transformedData.push(ObjectSerializer.deserialize(date, subType, format));
            }
            return transformedData;
        } else if (type.startsWith(mapPrefix)) {
            let subType: string = type.slice(mapPrefix.length, -mapSuffix.length); // { [key: string]: Type; } => Type
            let transformedData: { [key: string]: any } = {};
            for (let key in data) {
                transformedData[key] = ObjectSerializer.deserialize(
                    data[key],
                    subType,
                    format,
                );
            }
            return transformedData;
        } else if (type === "Date") {
            return new Date(data);
        } else {
            if (enumsMap.has(type)) {// is Enum
                return data;
            }

            if (!typeMap[type]) { // dont know the type
                return data;
            }
            let instance = new typeMap[type]();
            let attributeTypes = typeMap[type].getAttributeTypeMap();
            for (let attributeType of attributeTypes) {
                let value = ObjectSerializer.deserialize(data[attributeType.baseName], attributeType.type, attributeType.format);
                if (value !== undefined) {
                    instance[attributeType.name] = value;
                }
            }
            return instance;
        }
    }


    /**
     * Normalize media type
     *
     * We currently do not handle any media types attributes, i.e. anything
     * after a semicolon. All content is assumed to be UTF-8 compatible.
     */
    public static normalizeMediaType(mediaType: string | undefined): string | undefined {
        if (mediaType === undefined) {
            return undefined;
        }
        return (mediaType.split(";")[0] ?? '').trim().toLowerCase();
    }

    /**
     * From a list of possible media types, choose the one we can handle best.
     *
     * The order of the given media types does not have any impact on the choice
     * made.
     */
    public static getPreferredMediaType(mediaTypes: Array<string>): string {
        /** According to OAS 3 we should default to json */
        if (mediaTypes.length === 0) {
            return "application/json";
        }

        const normalMediaTypes = mediaTypes.map(ObjectSerializer.normalizeMediaType);

        for (const predicate of supportedMimeTypePredicatesWithPriority) {
            for (const mediaType of normalMediaTypes) {
                if (mediaType != null && predicate(mediaType)) {
                    return mediaType;
                }
            }
        }

        throw new Error("None of the given media types are supported: " + mediaTypes.join(", "));
    }

    /**
     * Convert data to a string according the given media type
     */
    public static stringify(data: any, mediaType: string): string {
        if (isTextLikeMimeType(mediaType)) {
            return String(data);
        }

        if (isJsonLikeMimeType(mediaType)) {
            return JSON.stringify(data);
        }

        throw new Error("The mediaType " + mediaType + " is not supported by ObjectSerializer.stringify.");
    }

    /**
     * Parse data from a string according to the given media type
     */
    public static parse(rawData: string, mediaType: string | undefined) {
        if (mediaType === undefined) {
            throw new Error("Cannot parse content. No Content-Type defined.");
        }

        if (isTextLikeMimeType(mediaType)) {
            return rawData;
        }

        if (isJsonLikeMimeType(mediaType)) {
            return JSON.parse(rawData);
        }

        throw new Error("The mediaType " + mediaType + " is not supported by ObjectSerializer.parse.");
    }
}
