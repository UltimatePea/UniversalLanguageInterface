# Universal Language Interface [![Build Status](https://travis-ci.org/UltimatePea/UniversalLanguageInterface.svg?branch=master)](https://travis-ci.org/UltimatePea/UniversalLanguageInterface)

This project facilitates calling a routine from another language by means of forking and calling via commandline. 
It is designed to eliminate the underlying procedures of dealing with process creation, piping outputs, waiting and closing process etc. 

## Supported Languages

- Python (Minimum Done)
- Haskell (Minimum Done)
- Nodejs (Blocked by a bug, waiting for time to debug)

## Calling Matrix

| Caller >> <br> Callee vv | Python | Haskell | Javascript |
| ---                      | ---    | ---     | ---        |
| Python                   | ✔      | ✔       | ✘          |
| Haskell                  | ✔      | ✔       | ✘          |
| Javascript               | ✘      | ✘       | ✘          |

## Complete Documentation

Please visit the [Wiki Page](https://github.com/UltimatePea/UniversalLanguageInterface/wiki).


