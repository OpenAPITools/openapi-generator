/* tslint:disable */
/* eslint-disable */

import {fromJS as originalFromJS, isIndexed, List, Map as ImmMap, RecordOf} from 'immutable';
import {normalize, NormalizedSchema, schema, Schema} from "normalizr";
import {ActionDefinition, createAction} from "redux-ts-simple";

export const knownRecordFactories = new Map<string, any>();
export const knownIndexedSetByKey: (string | number)[] = [];

export function appFromJS(any: any): any {
    return originalFromJS(any, (key, value) => {
        if (isIndexed(value)) {
            return knownIndexedSetByKey.indexOf(key) !== -1 ? value.toSet() : value.toList();
        } // we're reviving an array -> it's a List
        const MatchingType = knownRecordFactories.get(value.get('recType')) as { new(input?: any): any }; // check if we know a Record with this type
        if (MatchingType) {
            return new MatchingType(value);
        }
        return value.toMap(); // no matching Record type found -> it's a plain old Map
    });
}

export type NormalizedRecordEntity = NormalizedSchema<{ [key: string]: Map<string, RecordOf<any>> }, string>;
export type NormalizedRecordEntities = NormalizedSchema<{ [key: string]: Map<string, RecordOf<any>> }, List<string>>;

export abstract class ApiRecordUtils<TAPI, TREC extends RecordOf<any>> {
    public abstract normalize(apiObject: TAPI, asEntity?: boolean): any;

    public getSchema(): Schema {
        console.log("Entity mode not supported on this record.");
        return new schema.Entity("entityNotSupported");
    }

    public normalizeArray(apiObjectArray: TAPI[], asEntity?: boolean): TAPI[] {
        apiObjectArray.forEach(apiObject => this.normalize(apiObject, asEntity));
        return apiObjectArray;
    }

    public normalizeAsEntities(apiObject: TAPI): NormalizedSchema<any, any> {
        const normalized = this.normalize(apiObject, true);
        return normalize(normalized, this.getSchema());
    }

    public normalizeArrayAsEntities(apiObject: TAPI[]): NormalizedSchema<any, any> {
        const normalized = this.normalizeArray(apiObject, true);
        return normalize(normalized, new schema.Array(this.getSchema()));
    }

    public fromApi(apiObject: TAPI): TREC {
        return appFromJS(this.normalize(apiObject));
    }

    public fromApiArray(apiObjectArray: TAPI[]): List<TREC> {
        this.normalizeArray(apiObjectArray);
        return appFromJS(apiObjectArray);
    }

    public fromApiAsEntities(apiObject: TAPI): NormalizedRecordEntity {
        return ApiRecordUtils.toNormalizedRecordEntities(this.normalizeAsEntities(apiObject), false);
    }

    public fromApiArrayAsEntities(apiObject: TAPI[]): NormalizedRecordEntities {
        return ApiRecordUtils.toNormalizedRecordEntities(this.normalizeArrayAsEntities(apiObject), true);
    }

    public toApi(record: TREC): TAPI {
        const apiObject = record.toJS();
        delete apiObject.recType;
        return apiObject;
    }

    public toApiArray(records: List<TREC>): TAPI[] {
        return records.map(record => this.toApi(record)).toArray();
    }

    public static toNormalizedRecordEntities(normalizedAsEntities: any, forArray: boolean) {
        const entities = normalizedAsEntities.entities;
        for (const entityKey of Object.keys(entities)) {
            entities[entityKey] = appFromJS(entities[entityKey]);
        }
        normalizedAsEntities.result = appFromJS(normalizedAsEntities.result || (forArray ? "" : []));
        return normalizedAsEntities;
    }
}

export const allApiActionFailures: SagaActionDefinition<any>[] = [];

export interface BaseEntitySupportPayloadApiAction {
    toInlined?: boolean;
    toEntities?: boolean;
    markErrorsAsHandled?: boolean;
}

export interface BasePayloadApiAction {
    markErrorsAsHandled?: boolean;
}

export interface SagaActionDefinition<TPayload> extends ActionDefinition<TPayload> {
    toString: () => string;
}

export function createSagaAction<TPayload>(type: string, options?: { doNotAutoRegisterFailure?: boolean, namespace?: string }): SagaActionDefinition<TPayload> {
    const {doNotAutoRegisterFailure, namespace} = options || {} as any;
    let actionDefinition = createAction<TPayload>(namespace ? `${namespace}-${type}` : type);
    (actionDefinition as any).toString = () => actionDefinition.type;
    if (type.endsWith("Failure") && !doNotAutoRegisterFailure) {
        allApiActionFailures.push(actionDefinition);
    }
    return actionDefinition;
}

export let apiCall: <Ctx, Fn extends (this: Ctx, ...args: any[]) => any>(context: Ctx, fn: Fn, ...args: Parameters<Fn>) => Generator<any, any, any>;

export function setApiCall(apiCallFc: <Ctx, Fn extends (this: Ctx, ...args: any[]) => any>(context: Ctx, fn: Fn, ...args: Parameters<Fn>) => Generator<any, any, any>) {
    console.log("init apiCall");
    apiCall = apiCallFc;
}

export const normalizedEntities = createSagaAction<NormalizedRecordEntities>("normalizedEntities");