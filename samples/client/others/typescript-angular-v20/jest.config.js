const { createDefaultPreset } = require("ts-jest");

const tsJestTransformCfg = createDefaultPreset().transform;

module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  testMatch: ['**/+(*.)+(spec).ts'],
  moduleFileExtensions: ['ts', 'js', 'json'],
  transform: {
    ...tsJestTransformCfg,
  },
};
