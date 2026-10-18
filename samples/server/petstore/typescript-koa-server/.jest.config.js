module.exports = {
  verbose: true,
  testEnvironment: 'node',
  moduleFileExtensions: ['ts', 'js'],
  testPathIgnorePatterns: ['/dist/'],
  transform: {
    '^.+\\.[t|j]s?$': ['ts-jest'],
  },
  transformIgnorePatterns: ['<rootDir>/node_modules/'],
  testRegex: '.*\\.test\\.(j|t)s?$',
  moduleNameMapper: {
    'tests/(.*)$': '<rootDir>/tests/$1',
    'configs/(.*)$': '<rootDir>/configs/$1',
    'app/(.*)$': '<rootDir>/app/$1',
    server: '<rootDir>/app.ts',
    // app: '<rootDir>/app.ts',
  },
}
