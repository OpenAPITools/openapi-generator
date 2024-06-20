const _ = require('lodash')

const replacePathParameters = (url) => url.replace(/{([^{}]+)}/g, (keyExpr, key) => `{{bundle.inputData.${key}}}`)
const childMapping = (objectsArray, prefix, model) => objectsArray ? objectsArray.map(object => model.mapping({inputData: object}, prefix)) : undefined
const removeIfEmpty = (obj) => _.isEmpty(JSON.parse(JSON.stringify(obj))) ? undefined : obj
const buildKeyAndLabel = (prefix, isInput = true, isArrayChild = false) => {
    const keyPrefix = !_.isEmpty(prefix) && (!isArrayChild || isInput) ? `${prefix}${isInput ? '.' : '__'}` : prefix
    const labelPrefix = !_.isEmpty(keyPrefix) ? keyPrefix.replaceAll('__', '.') : ''
    return {
        keyPrefix: keyPrefix,
        labelPrefix: labelPrefix,
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

const requestOptionsMiddleware = (z, bundle, requestOptions) => {
  // TODO: modify the request options for all outgoing request to your api
  //       if you are using session authentication without a Bearer token.
  //       This may be true if your API uses basic authentication or api keys.
  return requestOptions
}

module.exports = {
    replacePathParameters: replacePathParameters,
    childMapping: childMapping,
    removeIfEmpty: removeIfEmpty,
    buildKeyAndLabel: buildKeyAndLabel,
    hasSearchRequisites: hasSearchRequisites,
    isSearchAction: isSearchAction,
    searchMiddleware: searchMiddleware,
    requestOptionsMiddleware: requestOptionsMiddleware,
}
