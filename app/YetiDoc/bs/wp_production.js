module.exports = {
  entry: './lib/js/src/start.bs.js',
  output: {
    path: __dirname +'/www',
    filename: 'index.js',
  },
  mode: 'production',  // development or production or none
};
