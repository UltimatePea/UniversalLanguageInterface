module.exports = {
	callSingle: callSingle,
	exportAndStart: exportAndStart
}

function callSingle(progName, cmdargs, functionName, args) {
}

/**
 * Arguments of type : JavaScript Object
 * [String : function(String):String]
 */
function exportAndStart(map) {

	// we parse command line arguments first
	var args = parseCommandLineArguments();
	if(args.mode == 'single') {
		startSingle(args, map);
	} else {
		throw new Error("Argument Modes other than `single` are not supported yet.")
	}

}

/**
 * Start single mode
 */
function startSingle(args, map) {
	const FIFO = require('fifo-js')
	console.log("Begin Fifo reads");
	var input_fifo = new FIFO(args.input_pipe);
	var output_fifo = new FIFO(args.output_pipe);
	// codes may throw from now onwards
	try{
		console.log("reading input fifo");
		var input_str = fifo.readSync();
		console.log("Done reading input fifo");
		var input_obj = JSON.parse(input_str);
		var input_name = input_obj.name;
		var input_arg = input_obj.args;
		var result = map.input_name(input_arg);
		var return_obj = {
			code : 200, 
			message : result
		}

		output_fifo.writeSync(JSON.stringify(return_obj, false));
	} catch(ex) {
		var exception = {
			code : 500,
			message: ex.message
		}
		//we need to flush the buffer (\n), with trim = false
		output_fifo.write(JSON.stringify(exception)+"\n", false); 
	}
}






/**
 * Parses and returns the command line arguments, callee
 */
function parseCommandLineArguments() {
	var ArgumentParser = require('argparse').ArgumentParser;
	var parser = new ArgumentParser({
	  version: '0.0.1',
	  addHelp:true,
	  description: 'Universal Language Interface Callee Program'
	});

	parser.addArgument(
	  [ '--mode' ],
	  {
		help: 'the mode of calling program',
		required : true
	  }
	);
	parser.addArgument(
	  [ '--input-pipe' ],
	  {
		help: 'the file path for caller -> callee unix named pipe',
		required : true
	  }
	);
	parser.addArgument(
	  [ '--output-pipe' ],
	  {
		help: 'the file path for callee -> caller unix named pipe',
		required : true
	  }
	);

	var args = parser.parseArgs();

	return args;
}
	
