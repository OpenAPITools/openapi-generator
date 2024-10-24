class_name GutHookScript
# ------------------------------------------------------------------------------
# This script is the base for custom scripts to be used in pre and post
# run hooks.
#
# To use, inherit from this script and then implement the run method.
# ------------------------------------------------------------------------------
var JunitXmlExport = load('res://addons/gut/junit_xml_export.gd')

# This is the instance of GUT that is running the tests.  You can get
# information about the run from this object.  This is set by GUT when the
# script is instantiated.
var gut  = null

# the exit code to be used by gut_cmdln.  See set method.
var _exit_code = null

var _should_abort =  false

# Virtual method that will be called by GUT after instantiating
# this script.
func run():
	gut.logger.error("Run method not overloaded.  Create a 'run()' method in your hook script to run your code.")


# Set the exit code when running from the command line.  If not set then the
# default exit code will be returned (0 when no tests fail, 1 when any tests
# fail).
func set_exit_code(code):
	_exit_code  = code

func get_exit_code():
	return _exit_code

# Usable by pre-run script to cause the run to end AFTER the run() method
# finishes.  post-run script will not be ran.
func abort():
	_should_abort = true

func should_abort():
	return _should_abort
