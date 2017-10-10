const ULI = require('universal-language-interface')


ULI.callSingle("node", ["nodejs_callee.js"], "testFunc", "Caller :: nodejs -- ", function(result){
	console.log(result);
});

