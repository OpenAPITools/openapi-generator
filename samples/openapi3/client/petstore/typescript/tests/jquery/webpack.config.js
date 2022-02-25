const path = require('path');

module.exports = {
  entry: './tests.ts',
  mode: "development",
  module: {
    rules: [
      {
        test: /\.png$/,
        use: 'raw-loader'
      },
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/
      }
    ]
  },
  resolve: {
    extensions: [ '.tsx', '.ts', '.js' ]
  },
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist')
  }
};