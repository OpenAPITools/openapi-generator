const config = require('./config');
const logger = require('./logger');
const ExpressServer = require('./expressServer');
// const App = require('./app');

// const app = new App(config);
// app.launch()
//   .then(() => {
//     logger.info('Server launched');
//   })
//   .catch((error) => {
//     logger.error('found error, shutting down server');
//     app.close()
//       .catch(closeError => logger.error(closeError))
//       .finally(() => logger.error(error));
//   });
const launchServer = async () => {
  try {
    this.expressServer = new ExpressServer(config.URL_PORT, config.OPENAPI_YAML);
    await this.expressServer.launch();
    logger.info('Express server running');
  } catch (error) {
    logger.error(error);
    await this.close();
  }
};

launchServer().catch(e => logger.error(e));
