const refmt = require('./bspacks/build/refmt_pre_closure');

console.log(refmt.getBindings('test', 'const nano = Nano("test", 4);'));