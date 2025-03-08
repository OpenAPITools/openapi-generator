# ##############################################################################
#(G)odot (U)nit (T)est class
#
# ##############################################################################
# The MIT License (MIT)
# =====================
#
# Copyright (c) 2020 Tom "Butch" Wesley
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
# ##############################################################################
# Description
# -----------
# Command line interface for the GUT unit testing tool.  Allows you to run tests
# from the command line instead of running a scene.  Place this script along with
# gut.gd into your scripts directory at the root of your project.  Once there you
# can run this script (from the root of your project) using the following command:
# 	godot -s -d test/gut/gut_cmdln.gd
#
# See the readme for a list of options and examples.  You can also use the -gh
# option to get more information about how to use the command line interface.
# ##############################################################################

#-------------------------------------------------------------------------------
# Parses the command line arguments supplied into an array that can then be
# examined and parsed based on how the gut options work.
#-------------------------------------------------------------------------------
class CmdLineParser:
	var _used_options = []
	# an array of arrays.  Each element in this array will contain an option
	# name and if that option contains a value then it will have a sedond
	# element.  For example:
	# 	[[-gselect, test.gd], [-gexit]]
	var _opts = []

	func _init():
		for i in range(OS.get_cmdline_args().size()):
			var opt_val = OS.get_cmdline_args()[i].split('=')
			_opts.append(opt_val)

	# Parse out multiple comma delimited values from a command line
	# option.  Values are separated from option name with "=" and
	# additional values are comma separated.
	func _parse_array_value(full_option):
		var value = _parse_option_value(full_option)
		var split = value.split(',')
		return split

	# Parse out the value of an option.  Values are separated from
	# the option name with "="
	func _parse_option_value(full_option):
		if(full_option.size() > 1):
			return full_option[1]
		else:
			return null

	# Search _opts for an element that starts with the option name
	# specified.
	func find_option(name):
		var found = false
		var idx = 0

		while(idx < _opts.size() and !found):
			if(_opts[idx][0] == name):
				found = true
			else:
				idx += 1

		if(found):
			return idx
		else:
			return -1

	func get_array_value(option):
		_used_options.append(option)
		var to_return = []
		var opt_loc = find_option(option)
		if(opt_loc != -1):
			to_return = _parse_array_value(_opts[opt_loc])
			_opts.remove_at(opt_loc)

		return to_return

	# returns the value of an option if it was specified, null otherwise.  This
	# used to return the default but that became problemnatic when trying to
	# punch through the different places where values could be specified.
	func get_value(option):
		_used_options.append(option)
		var to_return = null
		var opt_loc = find_option(option)
		if(opt_loc != -1):
			to_return = _parse_option_value(_opts[opt_loc])
			_opts.remove_at(opt_loc)

		return to_return

	# returns true if it finds the option, false if not.
	func was_specified(option):
		_used_options.append(option)
		return find_option(option) != -1

	# Returns any unused command line options.  I found that only the -s and
	# script name come through from godot, all other options that godot uses
	# are not sent through OS.get_cmdline_args().
	#
	# This is a onetime thing b/c i kill all items in _used_options
	func get_unused_options():
		var to_return = []
		for i in range(_opts.size()):
			to_return.append(_opts[i][0])

		var script_option = to_return.find("-s")
		if script_option == -1:
			script_option = to_return.find("--script")
		if script_option != -1:
			to_return.remove_at(script_option + 1)
			to_return.remove_at(script_option)

		while(_used_options.size() > 0):
			var index = to_return.find(_used_options[0].split("=")[0])
			if(index != -1):
				to_return.remove_at(index)
			_used_options.remove_at(0)

		return to_return

#-------------------------------------------------------------------------------
# Simple class to hold a command line option
#-------------------------------------------------------------------------------
class Option:
	var value = null
	var option_name = ''
	var default = null
	var description = ''

	func _init(name,default_value,desc=''):
		option_name = name
		default = default_value
		description = desc
		value = null#default_value

	func pad(to_pad, size, pad_with=' '):
		var to_return = to_pad
		for _i in range(to_pad.length(), size):
			to_return += pad_with

		return to_return

	func to_s(min_space=0):
		var subbed_desc = description
		if(subbed_desc.find('[default]') != -1):
			subbed_desc = subbed_desc.replace('[default]', str(default))
		return pad(option_name, min_space) + subbed_desc

#-------------------------------------------------------------------------------
# The high level interface between this script and the command line options
# supplied.  Uses Option class and CmdLineParser to extract information from
# the command line and make it easily accessible.
#-------------------------------------------------------------------------------
var options = []
var _opts = []
var _banner = ''

func add(name, default, desc):
	options.append(Option.new(name, default, desc))

func get_value(name):
	var found = false
	var idx = 0

	while(idx < options.size() and !found):
		if(options[idx].option_name == name):
			found = true
		else:
			idx += 1

	if(found):
		return options[idx].value
	else:
		print("COULD NOT FIND OPTION " + name)
		return null

func set_banner(banner):
	_banner = banner

func print_help():
	var longest = 0
	for i in range(options.size()):
		if(options[i].option_name.length() > longest):
			longest = options[i].option_name.length()

	print('---------------------------------------------------------')
	print(_banner)

	print("\nOptions\n-------")
	for i in range(options.size()):
		print('  ' + options[i].to_s(longest + 2))
	print('---------------------------------------------------------')

func print_options():
	for i in range(options.size()):
		print(options[i].option_name + '=' + str(options[i].value))

func parse():
	var parser = CmdLineParser.new()

	for i in range(options.size()):
		var t = typeof(options[i].default)
		# only set values that were specified at the command line so that
		# we can punch through default and config values correctly later.
		# Without this check, you can't tell the difference between the
		# defaults and what was specified, so you can't punch through
		# higher level options.
		if(parser.was_specified(options[i].option_name)):
			if(t == TYPE_INT):
				options[i].value = int(parser.get_value(options[i].option_name))
			elif(t == TYPE_STRING):
				options[i].value = parser.get_value(options[i].option_name)
			elif(t == TYPE_ARRAY):
				options[i].value = parser.get_array_value(options[i].option_name)
			elif(t == TYPE_BOOL):
				options[i].value = parser.was_specified(options[i].option_name)
			elif(t == TYPE_FLOAT):
				options[i].value = parser.get_value(options[i].option_name)
			elif(t == TYPE_NIL):
				print(options[i].option_name + ' cannot be processed, it has a nil datatype')
			else:
				print(options[i].option_name + ' cannot be processed, it has unknown datatype:' + str(t))

	var unused = parser.get_unused_options()
	if(unused.size() > 0):
		print("Unrecognized options:  ", unused)
		return false

	return true
