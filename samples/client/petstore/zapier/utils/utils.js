const _ = require('lodash')

const replacePathParameters = (url) => url.replace(/{([^{}]+)}/g, (keyExpr, key) => `{{bundle.inputData.${key}}}`)
const removeKeyPrefixes = (objectsArray) => objectsArray == undefined || typeof objectsArray[0] != 'object' ? objectsArray : objectsArray.map((obj) => Object.keys(obj).reduce((res, key) => (res[(key.split('.')).slice(-1)] = obj[key], res), {}))
const removeIfEmpty = (obj) => _.isEmpty(JSON.parse(JSON.stringify(obj))) ? undefined : obj
const hasASearchField = action => action.operation.inputFields.length > 0
const isSearchAction = (key) => {
    // TODO: custom logic
    return false
}
const searchMiddleware = (action) => {
    // TODO: custom logic
    return action
}

module.exports = {
    replacePathParameters: replacePathParameters,
    removeKeyPrefixes: removeKeyPrefixes,
    removeIfEmpty: removeIfEmpty,
    hasASearchField: hasASearchField,
    isSearchAction: isSearchAction,
    searchMiddleware: searchMiddleware,
}