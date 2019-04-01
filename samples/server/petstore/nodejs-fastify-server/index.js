const openapiGlue = require("index.js");

const options = {
  specification: `${__dirname}/openapi.json`,
  service: `${__dirname}/service.js`
};

module.exports = async function(fastify, opts) {
  fastify.register(openapiGlue, options);
};
