const _ = require('lodash')

const replacePathParameters = (url) => url.replace(/{([^{}]+)}/g, (keyExpr, key) => `{{bundle.inputData.${key}}}`)
const removeKeyPrefixes = (objectsArray, currentPath) => objectsArray == undefined || typeof objectsArray[0] != 'object' ? objectsArray : objectsArray.map((obj) => Object.keys(obj).reduce((res, key) =>_.set(res, key.replace(`${currentPath}.`, ''), obj[key]), {}))
const removeIfEmpty = (obj) => _.isEmpty(JSON.parse(JSON.stringify(obj))) ? undefined : obj
const buildKeyAndLabel = (prefix, isInput = true, isArrayChild = false) => {
    const keyPrefix = !_.isEmpty(prefix) && (!isArrayChild || isInput) ? `${prefix}${isInput ? '.' : '__'}` : prefix
    const labelPrefix = !_.isEmpty(keyPrefix) ? keyPrefix.replaceAll('__', '.') : ''
    return {
        keyPrefix: keyPrefix,
        labelPrefix:labelPrefix,
    }
}
const isSearchAction = (key) => {
    // TODO: custom logic
    return false
}
const hasASearchField = action => action.operation.inputFields.length > 0
const returnsObjectsArray = action => !!action.operation.outputFields.find(field => 'children' in field)
const hasSearchRequisites = action => hasASearchField(action) && returnsObjectsArray(action)
const searchMiddleware = (action) => {
    // TODO: custom logic
    return action
}

module.exports = {
    replacePathParameters: replacePathParameters,
    removeKeyPrefixes: removeKeyPrefixes,
    removeIfEmpty: removeIfEmpty,
    buildKeyAndLabel: buildKeyAndLabel,
    hasSearchRequisites: hasSearchRequisites,
    isSearchAction: isSearchAction,
    searchMiddleware: searchMiddleware,
}