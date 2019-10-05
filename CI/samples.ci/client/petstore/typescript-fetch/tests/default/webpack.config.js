const webpack = require('webpack');

module.exports = {
  entry: {
    app: './test/index.ts',
    vendor: [
      // libraries
      '@swagger/typescript-fetch-petstore'
    ],
  },
  output: {
    filename: './dist/test.webpack-bundle.js'
  },
  plugins: [
    new webpack.optimize.CommonsChunkPlugin(/* chunkName= */'vendor', /* filename= */'./dist/vendor.webpack-bundle.js')
  ],
  resolve: {
    // Add `.ts` and `.tsx` as a resolvable extension.
    extensions: ['', '.webpack.js', '.web.js', '.ts', '.tsx', '.js']
  },
  module: {
    loaders: [
      // all files with a `.ts` or `.tsx` extension will be handled by `ts-loader`
      {
        test: /\.tsx?$/,
        loader: 'ts-loader'
      }
    ]
  }
};
