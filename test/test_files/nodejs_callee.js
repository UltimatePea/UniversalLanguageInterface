
const ULI = require('universal-language-interface')

const nodejs_test_functions = require('./nodejs_test_functions')

console.log("Node js callee listening");
ULI.exportAndStart({
        "test_func": nodejs_test_functions.test_func
    })
