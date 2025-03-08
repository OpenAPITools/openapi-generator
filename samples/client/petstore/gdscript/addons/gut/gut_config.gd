var Gut = load('res://addons/gut/gut.gd')

# Do not want a ref to _utils here due to use by editor plugin.
# _utils needs to be split so that constants and what not do not
# have to rely on the weird singleton thing I made.
enum DOUBLE_STRATEGY{
	SCRIPT_ONLY,
	INCLUDE_SUPER
}


var valid_fonts = ['AnonymousPro', 'CourierPro', 'LobsterTwo', 'Default']
var default_options = {
	background_color = Color(.15, .15, .15, 1).to_html(),
	config_file = 'res://.gutconfig.json',
	dirs = [],
	disable_colors = false,
	double_strategy = 'partial',
	font_color = Color(.8, .8, .8, 1).to_html(),
	font_name = 'CourierPrime',
	font_size = 16,
	hide_orphans = false,
	ignore_pause = false,
	include_subdirs = false,
	inner_class = '',
	junit_xml_file = '',
	junit_xml_timestamp = false,
	log_level = 1,
	opacity = 100,
	paint_after = .1,
	post_run_script = '',
	pre_run_script = '',
	prefix = 'test_',
	selected = '',
	should_exit = false,
	should_exit_on_success = false,
	should_maximize = false,
	compact_mode = false,
	show_help = false,
	suffix = '.gd',
	tests = [],
	unit_test_name = '',

	gut_on_top = true,
}

var default_panel_options = {
	font_name = 'CourierPrime',
	font_size = 30,
	hide_result_tree = false,
	hide_output_text = false,
	hide_settings = false,
	use_colors = true
}

var options = default_options.duplicate()
var json = JSON.new()


func _null_copy(h):
	var new_hash = {}
	for key in h:
		new_hash[key] = null
	return new_hash


func _load_options_from_config_file(file_path, into):
	# SHORTCIRCUIT

	if(!FileAccess.file_exists(file_path)):
		if(file_path != 'res://.gutconfig.json'):
			print('ERROR:  Config File "', file_path, '" does not exist.')
			return -1
		else:
			return 1

	var f = FileAccess.open(file_path, FileAccess.READ)
	if(f == null):
		var result = FileAccess.get_open_error()
		push_error(str("Could not load data ", file_path, ' ', result))
		return result

	var json = f.get_as_text()
	f = null # close file

	var test_json_conv = JSON.new()
	test_json_conv.parse(json)
	var results = test_json_conv.get_data()
	# SHORTCIRCUIT
	if(results == null):
		print("\n\n",'!! ERROR parsing file:  ', file_path)
		print('    at line ', results.error_line, ':')
		print('    ', results.error_string)
		return -1

	# Get all the options out of the config file using the option name.  The
	# options hash is now the default source of truth for the name of an option.
	_load_dict_into(results, into)

	return 1

func _load_dict_into(source, dest):
	for key in dest:
		if(source.has(key)):
			if(source[key] != null):
				if(typeof(source[key]) == TYPE_DICTIONARY):
					_load_dict_into(source[key], dest[key])
				else:
					dest[key] = source[key]




func write_options(path):
	var content = json.stringify(options, ' ')

	var f = FileAccess.open(path, FileAccess.WRITE)
	var result = FileAccess.get_open_error()
	if(f != null):
		f.store_string(content)
		f = null # closes file
	else:
		print('ERROR:  could not open file ', path, ' ', result)
	return result


# Apply all the options specified to _tester.  This is where the rubber meets
# the road.
func _apply_options(opts, _tester):
	_tester.include_subdirectories = opts.include_subdirs

	if(opts.inner_class != ''):
		_tester.inner_class_name = opts.inner_class
	_tester.log_level = opts.log_level
	_tester.ignore_pause_before_teardown = opts.ignore_pause

	if(opts.selected != ''):
		_tester.select_script(opts.selected)

	for i in range(opts.dirs.size()):
		_tester.add_directory(opts.dirs[i], opts.prefix, opts.suffix)

	for i in range(opts.tests.size()):
		_tester.add_script(opts.tests[i])

	if(opts.double_strategy == 'include super'):
		_tester.double_strategy = DOUBLE_STRATEGY.INCLUDE_SUPER
	elif(opts.double_strategy == 'script only'):
		_tester.double_strategy = DOUBLE_STRATEGY.SCRIPT_ONLY

	_tester.unit_test_name = opts.unit_test_name
	_tester.pre_run_script = opts.pre_run_script
	_tester.post_run_script = opts.post_run_script
	_tester.color_output = !opts.disable_colors
	_tester.show_orphans(!opts.hide_orphans)
	_tester.junit_xml_file = opts.junit_xml_file
	_tester.junit_xml_timestamp = opts.junit_xml_timestamp
	_tester.paint_after = str(opts.paint_after).to_float()

	return _tester


func config_gut(gut):
	return _apply_options(options, gut)


func load_options(path):
	return _load_options_from_config_file(path, options)

func load_panel_options(path):
	options['panel_options'] = default_panel_options.duplicate()
	return _load_options_from_config_file(path, options)

func load_options_no_defaults(path):
	options = _null_copy(default_options)
	return _load_options_from_config_file(path, options)

func apply_options(gut):
	_apply_options(options, gut)
