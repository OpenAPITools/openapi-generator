const authentication = require('./authentication');
const { searchActions, createActions, triggers } = require('./operations/actions');

module.exports = {
    version: require('./package.json').version,
    platformVersion: require('zapier-platform-core').version,
    authentication: authentication,
    searches: searchActions(),
    creates: createActions(),
    triggers: triggers(),
};
