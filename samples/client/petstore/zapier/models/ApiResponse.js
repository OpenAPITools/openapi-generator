const _ = require('lodash')
const utils = require('../utils/utils');

module.exports = {
    fields: (prefix = '', isInput = true) => {
        let keyPrefix = prefix && `${prefix}${isInput ? '.' : '__'}`
        let labelPrefix = keyPrefix && keyPrefix.replaceAll('__', '.')
        return [
            {
                key: `${keyPrefix}code`,
                label: `[${labelPrefix}code]`,
                type: 'integer',
            },
            {
                key: `${keyPrefix}type`,
                label: `[${labelPrefix}type]`,
                type: 'string',
            },
            {
                key: `${keyPrefix}message`,
                label: `[${labelPrefix}message]`,
                type: 'string',
            },
        ]
    },
    mapping: (bundle, prefix = '') => {
        let keyPrefix = prefix && `${prefix}.`
        return {
            'code': bundle.inputData?.[`${keyPrefix}code`],
            'type': bundle.inputData?.[`${keyPrefix}type`],
            'message': bundle.inputData?.[`${keyPrefix}message`],
        }
    },
}
