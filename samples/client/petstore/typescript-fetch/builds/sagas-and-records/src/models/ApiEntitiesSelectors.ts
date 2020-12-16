import {ApiEntitiesRecord} from "./ApiEntitiesRecord"
import {createSelector} from "reselect";

let apiEntitiesSelector: (state: any) => ApiEntitiesRecord = (state: any) => state.apiEntities;

export function setApiEntitiesSelector(selector: (state: any) => ApiEntitiesRecord) { // Use this to customize the location where you have placed your ApiEntitiesRecord in your project.
    apiEntitiesSelector = selector;
}

export const apiEntitiesCategorySelector = createSelector(apiEntitiesSelector, apiEntities => apiEntities.category);
export const apiEntitiesOrderSelector = createSelector(apiEntitiesSelector, apiEntities => apiEntities.order);
export const apiEntitiesPetSelector = createSelector(apiEntitiesSelector, apiEntities => apiEntities.pet);
export const apiEntitiesTagSelector = createSelector(apiEntitiesSelector, apiEntities => apiEntities.tag);
export const apiEntitiesUserSelector = createSelector(apiEntitiesSelector, apiEntities => apiEntities.user);
