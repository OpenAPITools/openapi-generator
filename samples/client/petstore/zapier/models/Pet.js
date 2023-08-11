const utils = require('../utils/utils');
const Category = require('../models/Category');
const Tag = require('../models/Tag');

module.exports = {
    fields: (prefix = '', isInput = true, isArrayChild = false) => {
        const {keyPrefix, labelPrefix} = utils.buildKeyAndLabel(prefix, isInput, isArrayChild)
        return [
            {
                key: `${keyPrefix}id`,
                label: `[${labelPrefix}id]`,
                type: 'number',
            },
            ...Category.fields(`${keyPrefix}category`, isInput),
            {
                key: `${keyPrefix}name`,
                label: `[${labelPrefix}name]`,
                type: 'string',
            },
            {
                key: `${keyPrefix}photoUrls`,
                label: `[${labelPrefix}photoUrls]`,
                list: true,
                type: 'string',
            },
            {
                key: `${keyPrefix}tags`,
                label: `[${labelPrefix}tags]`,
                children: Tag.fields(`${keyPrefix}tags${!isInput ? '[]' : ''}`, isInput, true), 
            },
            {
                key: `${keyPrefix}status`,
                label: `pet status in the store - [${labelPrefix}status]`,
                type: 'string',
                choices: [
                    'available',
                    'pending',
                    'sold',
                ],
            },
        ]
    },
    mapping: (bundle, prefix = '') => {
        const {keyPrefix} = utils.buildKeyAndLabel(prefix)
        return {
            'id': bundle.inputData?.[`${keyPrefix}id`],
            'category': utils.removeIfEmpty(Category.mapping(bundle, `${keyPrefix}category`)),
            'name': bundle.inputData?.[`${keyPrefix}name`],
            'photoUrls': bundle.inputData?.[`${keyPrefix}photoUrls`],
            'tags': utils.removeKeyPrefixes(bundle.inputData?.[`${keyPrefix}tags`], `${keyPrefix}tags`),
            'status': bundle.inputData?.[`${keyPrefix}status`],
        }
    },
}
