module.exports = {
	callSingle: callSingle,
	exportAndStart: exportAndStart
}

function callSingle(progName, cmdargs, functionName, args, callback) {

	const FIFO = require('fifo-js')

	var callerToCalleePipe = new FIFO();
	var calleeToCallerPipe = new FIFO();

	console.log("Pipes created");

	const { spawn } = require("child_process");

	var additionalCmdOptions = ["--mode", "single", "--input-pipe", calleeToCallerPipe.path, "--output-pipe", calleeToCallerPipe.path];
	console.log("Creating child process");
	const child = spawn(progName + " " +  Array.prototype.join.call(cmdargs + additionalCmdOptions, " "), {
		stdio: 'inherit', 
		detached: true
	});

	child.on('exit', function(code, signal){
		// this is called when child process terminates
		// read output
		console.log("child process finished. Reading from child process output pipe");
		var resp = calleeToCallerPipe.readSync();
		console.log("finished reading from child process output pipe");
		
		var respObj = JSON.parse(resp);

		if(respObj.code == 500) {
			throw new Error("Callee Internal Error");
		} else if (respObj.code == 200) {
			callback(respObj.return_val);
		} else {
			throw new Error("Unable to read data from child process");
		}

		console.log("All Done, clean up");
		callerToCalleePipe.close();
		calleeToCallerPipe.close();
	});

	console.log("Child process created Writing to input pipe");
	callerToCalleePipe.writeSync(JSON.stringify({
		name : functionName,
		args : args
	}) + "\n", false);
	console.log("finished writing to input pipe");



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
		console.log("Callee reading input fifo");
		var input_str = fifo.readSync();
		console.log("Callee Done reading input fifo");
		var input_obj = JSON.parse(input_str);
		var input_name = input_obj.name;
		var input_arg = input_obj.args;
		var result = map.input_name(input_arg);
		var return_obj = {
			code : 200, 
			message : result
		}
		console.log("Callee Writing output");
		output_fifo.writeSync(JSON.stringify(return_obj, false));
		console.log("Finished Writing output");

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
	
