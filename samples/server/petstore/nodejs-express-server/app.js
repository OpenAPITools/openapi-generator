const ExpressServer = require('./expressServer');
const logger = require('./logger');
// const swaggerRouter = require('./utils/swaggerRouter');

class App {
  constructor(config) {
    this.config = config;
  }

  async launch() {
    try {
      this.expressServer = new ExpressServer(this.config.URL_PORT, this.config.OPENAPI_YAML);
      // this.expressServer.app.use(swaggerRouter());
      await this.expressServer.launch();
      logger.info('Express server running');
    } catch (error) {
      logger.error(error);
      await this.close();
    }
  }

  async close() {
    if (this.expressServer !== undefined) {
      await this.expressServer.close();
      logger.info(`Server shut down on port ${this.config.URL_PORT}`);
    }
  }
}

module.exports = App;
