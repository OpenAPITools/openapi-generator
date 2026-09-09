/**
 * Wires the generated router factory into an Express app using the hand-written
 * service implementations. Exported as a factory so tests can create isolated
 * app instances.
 */
import express from "express";

import { getRouter } from "../builds/default/express";
import { authorsService, catalogService } from "./services";

export function createApp(): express.Express {
  const app = express();
  app.use(express.json());

  const router = getRouter({
    // Only consumed to serve Swagger UI at "/"; not needed for the API routes.
    schema: {},
    getAuthorsService: () => authorsService,
    getCatalogService: () => catalogService,
  });

  app.use(router);
  return app;
}
