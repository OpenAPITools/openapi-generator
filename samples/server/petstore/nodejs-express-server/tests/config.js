const path = require('path');

const config = {
  ROOT_DIR: path.join(__dirname, '../'),
  URL_PORT: 3009,
  URL_PATH: 'http://localhost',
  BASE_VERSION: 'v2',
};
config.OPENAPI_YAML = path.join(config.ROOT_DIR, 'api', 'openapi.yaml');
config.FULL_PATH = `${config.URL_PATH}:${config.URL_PORT}/${config.BASE_VERSION}`;

module.exports = config;
