# ------------------------------------------------------------------------------
# Used to keep track of info about each test ran.
# ------------------------------------------------------------------------------
class Test:
	# indicator if it passed or not.  defaults to true since it takes only
	# one failure to make it not pass.  _fail in gut will set this.
	var passed = true
	# the name of the function
	var name = ""
	# flag to know if the name has been printed yet.
	var has_printed_name = false
	# the number of arguments the method has
	var arg_count = 0
	# The number of asserts in the test
	var assert_count = 0
	# if the test has been marked pending at anypont during
	# execution.
	var pending = false
	# the line number when the  test fails
	var line_number = -1
	# Set this to true to prevent GUT from running the test.
	var should_skip = false

	func did_pass():
		return passed and !pending and assert_count > 0

	func did_assert():
		return assert_count > 0 or pending


# ------------------------------------------------------------------------------
# This holds all the meta information for a test script.  It contains the
# name of the inner class and an array of Test "structs".
#
# This class also facilitates all the exporting and importing of tests.
# ------------------------------------------------------------------------------
class TestScript:
	var inner_class_name:StringName
	var tests = []
	var path:String
	var _utils = null
	var _lgr = null
	var is_loaded = false

	func _init(utils=null,logger=null):
		_utils = utils
		_lgr = logger

	func to_s():
		var to_return = path
		if(inner_class_name != null):
			to_return += str('.', inner_class_name)
		to_return += "\n"
		for i in range(tests.size()):
			to_return += str('  ', tests[i].name, "\n")
		return to_return

	func get_new():
		return load_script().new()

	func load_script():
		var to_return = load(path)

		if(inner_class_name != null and inner_class_name != ''):
			# If we wanted to do inner classes in inner classses
			# then this would have to become some kind of loop or recursive
			# call to go all the way down the chain or this class would
			# have to change to hold onto the loaded class instead of
			# just path information.
			to_return = to_return.get(inner_class_name)

		return to_return

	func get_filename_and_inner():
		var to_return = get_filename()
		if(inner_class_name != ''):
			to_return += '.' + String(inner_class_name)
		return to_return

	func get_full_name():
		var to_return = path
		if(inner_class_name != ''):
			to_return += '.' + String(inner_class_name)
		return to_return

	func get_filename():
		return path.get_file()

	func has_inner_class():
		return inner_class_name != ''


	# Note:  although this no longer needs to export the inner_class names since
	#        they are pulled from metadata now, it is easier to leave that in
	#        so we don't have to cut the export down to unique script names.
	func export_to(config_file, section):
		config_file.set_value(section, 'path', path)
		config_file.set_value(section, 'inner_class', inner_class_name)
		var names = []
		for i in range(tests.size()):
			names.append(tests[i].name)
		config_file.set_value(section, 'tests', names)


	func _remap_path(source_path):
		var to_return = source_path
		if(!_utils.file_exists(source_path)):
			_lgr.debug('Checking for remap for:  ' + source_path)
			var remap_path = source_path.get_basename() + '.gd.remap'
			if(_utils.file_exists(remap_path)):
				var cf = ConfigFile.new()
				cf.load(remap_path)
				to_return = cf.get_value('remap', 'path')
			else:
				_lgr.warn('Could not find remap file ' + remap_path)
		return to_return


	func import_from(config_file, section):
		path = config_file.get_value(section, 'path')
		path = _remap_path(path)
		# Null is an acceptable value, but you can't pass null as a default to
		# get_value since it thinks you didn't send a default...then it spits
		# out red text.  This works around that.
		var inner_name = config_file.get_value(section, 'inner_class', 'Placeholder')
		if(inner_name != 'Placeholder'):
			inner_class_name = inner_name
		else: # just being explicit
			inner_class_name = StringName("")

	func get_test_named(name):
		return _utils.search_array(tests, 'name', name)

	func mark_tests_to_skip_with_suffix(suffix):
		for single_test in tests:
			single_test.should_skip = single_test.name.ends_with(suffix)



# ------------------------------------------------------------------------------
# start test_collector, I don't think I like the name.
# ------------------------------------------------------------------------------
var scripts = []
var _test_prefix = 'test_'
var _test_class_prefix = 'Test'

var _utils = load('res://addons/gut/utils.gd').get_instance()
var _lgr = _utils.get_logger()

func _does_inherit_from_test(thing):
	var base_script = thing.get_base_script()
	var to_return = false
	if(base_script != null):
		var base_path = base_script.get_path()
		if(base_path == 'res://addons/gut/test.gd'):
			to_return = true
		else:
			to_return = _does_inherit_from_test(base_script)
	return to_return


func _populate_tests(test_script:TestScript):
	var script =  test_script.load_script()
	if(script == null):
		print('  !!! ', test_script.path, ' could not be loaded')
		return false

	test_script.is_loaded = true
	var methods = script.get_script_method_list()
	for i in range(methods.size()):
		var name = methods[i]['name']
		if(name.begins_with(_test_prefix)):
			var t = Test.new()
			t.name = name
			t.arg_count = methods[i]['args'].size()
			test_script.tests.append(t)

func _get_inner_test_class_names(loaded):
	var inner_classes = []
	var const_map = loaded.get_script_constant_map()
	for key in const_map:
		var thing = const_map[key]
		if(_utils.is_gdscript(thing)):
			if(key.begins_with(_test_class_prefix)):
				if(_does_inherit_from_test(thing)):
					inner_classes.append(key)
				else:
					_lgr.warn(str('Ignoring Inner Class ', key,
						' because it does not extend res://addons/gut/test.gd'))

			# This could go deeper and find inner classes within inner classes
			# but requires more experimentation.  Right now I'm keeping it at
			# one level since that is what the previous version did and there
			# has been no demand for deeper nesting.
			# _populate_inner_test_classes(thing)
	return inner_classes

func _parse_script(test_script):
	var inner_classes = []
	var scripts_found = []

	var loaded = load(test_script.path)
	if(_does_inherit_from_test(loaded)):
		_populate_tests(test_script)
		scripts_found.append(test_script.path)
		inner_classes = _get_inner_test_class_names(loaded)

	for i in range(inner_classes.size()):
		var loaded_inner = loaded.get(inner_classes[i])
		if(_does_inherit_from_test(loaded_inner)):
			var ts = TestScript.new(_utils, _lgr)
			ts.path = test_script.path
			ts.inner_class_name = inner_classes[i]
			_populate_tests(ts)
			scripts.append(ts)
			scripts_found.append(test_script.path + '[' + inner_classes[i] +']')

	return scripts_found

# -----------------
# Public
# -----------------
func add_script(path):
	# print('Adding ', path)
	# SHORTCIRCUIT
	if(has_script(path)):
		return []

	# SHORTCIRCUIT
	if(!FileAccess.file_exists(path)):
		_lgr.error('Could not find script:  ' + path)
		return

	var ts = TestScript.new(_utils, _lgr)
	ts.path = path
	scripts.append(ts)
	return _parse_script(ts)

func clear():
	scripts.clear()

func has_script(path):
	var found = false
	var idx = 0
	while(idx < scripts.size() and !found):
		if(scripts[idx].get_full_name() == path):
			found = true
		else:
			idx += 1
	return found

func export_tests(path):
	var success = true
	var f = ConfigFile.new()
	for i in range(scripts.size()):
		scripts[i].export_to(f, str('TestScript-', i))
	var result = f.save(path)
	if(result != OK):
		_lgr.error(str('Could not save exported tests to [', path, '].  Error code:  ', result))
		success = false
	return success

func import_tests(path):
	var success = false
	var f = ConfigFile.new()
	var result = f.load(path)
	if(result != OK):
		_lgr.error(str('Could not load exported tests from [', path, '].  Error code:  ', result))
	else:
		var sections = f.get_sections()
		for key in sections:
			var ts = TestScript.new(_utils, _lgr)
			ts.import_from(f, key)
			_populate_tests(ts)
			scripts.append(ts)
		success = true
	return success

func get_script_named(name):
	return _utils.search_array(scripts, 'get_filename_and_inner', name)

func get_test_named(script_name, test_name):
	var s = get_script_named(script_name)
	if(s != null):
		return s.get_test_named(test_name)
	else:
		return null

func to_s():
	var to_return = ''
	for i in range(scripts.size()):
		to_return += scripts[i].to_s() + "\n"
	return to_return

# ---------------------
# Accessors
# ---------------------
func get_logger():
	return _lgr

func set_logger(logger):
	_lgr = logger

func get_test_prefix():
	return _test_prefix

func set_test_prefix(test_prefix):
	_test_prefix = test_prefix

func get_test_class_prefix():
	return _test_class_prefix

func set_test_class_prefix(test_class_prefix):
	_test_class_prefix = test_class_prefix
