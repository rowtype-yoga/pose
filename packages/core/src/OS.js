var fs = require('fs');
exports.stdinAsString = () => {
  return fs.readFileSync(0, 'utf-8');
}