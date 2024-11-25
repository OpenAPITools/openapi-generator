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

const isCreateAction = (key) => {
    // TODO: return true if the key is a "create" action for your API
    return !isSearchAction(key);
}

const createMiddleware = (action) => {
    return action
}

const isTrigger = (key) => {
    // TODO: custom logic
    return false
}

const triggerMiddleware = (action) => {
    return action
}

const requestOptionsMiddleware = (z, bundle, requestOptions) => {
  // TODO: modify the request options for all outgoing request to your api
  //       if you are using session authentication without a Bearer token.
  //       This may be true if your API uses basic authentication or api keys.
  return requestOptions
}

const responseOptionsMiddleware = (z, bundle, key, json) => {
  // TODO: modify if your response needs to be transformed before returning the
  //      data to Zapier. For example, you may need to map an id field to the
  //      "id" field. For example, map "contactId": 1 to "id": 1. Or wrap your
  //      response in a json object. For example, { data: response }.
  //
  //      Note that if the type being returned from the endpoint is a primitive
  //      type, the response is automatically wrapped like this: { data: response }.
  return json
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
    responseOptionsMiddleware: responseOptionsMiddleware,
    isTrigger: isTrigger,
    triggerMiddleware: triggerMiddleware,
    isCreateAction: isCreateAction,
    createMiddleware: createMiddleware,
}
