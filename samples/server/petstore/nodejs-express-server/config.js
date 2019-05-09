const path = require("path");
const config = {
  "ROOT_DIR": __dirname,
  "URL_PORT": 3000
};
config.OPENAPI_YAML = path.join(config.ROOT_DIR, "api", "openapi.yaml");

module.exports = config;