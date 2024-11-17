# ------------------------------------------------------------------------------
# Choose an existing directory from res://.  Dialog allows for creating a 
# directory.
# ------------------------------------------------------------------------------
class DirectoryCtrl:
	extends HBoxContainer

	var text = '':
		get:
			return _txt_path.text
		set(val):
			_txt_path.text = val

	var _txt_path := LineEdit.new()
	var _btn_dir := Button.new()
	var _dialog := FileDialog.new()

	func _init():
		_btn_dir.text = '...'
		_btn_dir.pressed.connect(_on_dir_button_pressed)

		_txt_path.size_flags_horizontal = _txt_path.SIZE_EXPAND_FILL

		_dialog.file_mode = _dialog.FILE_MODE_OPEN_DIR
		_dialog.unresizable = false
		_dialog.dir_selected.connect(_on_selected)
		_dialog.file_selected.connect(_on_selected)
		_dialog.size = Vector2(1000, 700)


	func _on_selected(path):
		text = path


	func _on_dir_button_pressed():
		_dialog.current_dir = _txt_path.text
		_dialog.popup_centered()


	func _ready():
		add_child(_txt_path)
		add_child(_btn_dir)
		add_child(_dialog)


	func get_line_edit():
		return _txt_path


# ------------------------------------------------------------------------------
# Choose an existing file in res://
# ------------------------------------------------------------------------------
class FileCtrl:
	extends DirectoryCtrl

	func _init():
		super._init()
		_dialog.file_mode = _dialog.FILE_MODE_OPEN_FILE


# ------------------------------------------------------------------------------
# Choose a save location.  Can pick anywhere on file system.  Will warn if you
# pick a file that already exists.
# ------------------------------------------------------------------------------
class SaveFileAnywhere:
	extends DirectoryCtrl

	func _init():
		super._init()
		_dialog.file_mode = _dialog.FILE_MODE_SAVE_FILE
		_dialog.access = _dialog.ACCESS_FILESYSTEM
	

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
class Vector2Ctrl:
	extends VBoxContainer

	var value = Vector2(-1, -1) :
		get:
			return get_value()
		set(val):
			set_value(val)
	var disabled = false :
		get:
			return get_disabled()
		set(val):
			set_disabled(val)
	var x_spin = SpinBox.new()
	var y_spin = SpinBox.new()

	func _init():
		add_child(_make_one('x:  ', x_spin))
		add_child(_make_one('y:  ', y_spin))

	func _make_one(txt, spinner):
		var hbox = HBoxContainer.new()
		var lbl = Label.new()
		lbl.text = txt
		hbox.add_child(lbl)
		hbox.add_child(spinner)
		spinner.min_value = -1
		spinner.max_value = 10000
		spinner.size_flags_horizontal = spinner.SIZE_EXPAND_FILL
		return hbox

	func set_value(v):
		if(v != null):
			x_spin.value = v[0]
			y_spin.value = v[1]

	# Returns array instead of vector2 b/c that is what is stored in
	# in the dictionary and what is expected everywhere else.
	func get_value():
		return [x_spin.value, y_spin.value]

	func set_disabled(should):
		get_parent().visible = !should
		x_spin.visible = !should
		y_spin.visible = !should

	func get_disabled():
		pass



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
var _base_container = null
var _base_control = null
const DIRS_TO_LIST = 6
var _cfg_ctrls = {}
var _avail_fonts = ['AnonymousPro', 'CourierPrime', 'LobsterTwo', 'Default']


func _init(cont):
	_base_container = cont

	_base_control = HBoxContainer.new()
	_base_control.size_flags_horizontal = _base_control.SIZE_EXPAND_FILL
	_base_control.mouse_filter = _base_control.MOUSE_FILTER_PASS

	# I don't remember what this is all about at all.  Could be
	# garbage.  Decided to spend more time typing this message
	# than figuring it out.
	var lbl = Label.new()
	lbl.size_flags_horizontal = lbl.SIZE_EXPAND_FILL
	lbl.mouse_filter = lbl.MOUSE_FILTER_STOP
	_base_control.add_child(lbl)


# ------------------
# Private
# ------------------
func _new_row(key, disp_text, value_ctrl, hint):
	var ctrl = _base_control.duplicate()
	var lbl = ctrl.get_child(0)

	lbl.tooltip_text = hint
	lbl.text = disp_text
	_base_container.add_child(ctrl)

	_cfg_ctrls[key] = value_ctrl
	ctrl.add_child(value_ctrl)

	var rpad = CenterContainer.new()
	# rpad.custom_minimum_size.x = 5
	ctrl.add_child(rpad)

	return ctrl


func _add_title(text):
	var row = _base_control.duplicate()
	var lbl = row.get_child(0)

	lbl.text = text
	# lbl.align = Label.ALIGNMENT_CENTER
	_base_container.add_child(row)

	row.connect('draw', _on_title_cell_draw.bind(row))


func _add_number(key, value, disp_text, v_min, v_max, hint=''):
	var value_ctrl = SpinBox.new()
	value_ctrl.value = value
	value_ctrl.size_flags_horizontal = value_ctrl.SIZE_EXPAND_FILL
	value_ctrl.min_value = v_min
	value_ctrl.max_value = v_max
	_wire_select_on_focus(value_ctrl.get_line_edit())

	return _new_row(key, disp_text, value_ctrl, hint)


func _add_select(key, value, values, disp_text, hint=''):
	var value_ctrl = OptionButton.new()
	var select_idx = 0
	for i in range(values.size()):
		value_ctrl.add_item(values[i])
		if(value == values[i]):
			select_idx = i
	value_ctrl.selected = select_idx
	value_ctrl.size_flags_horizontal = value_ctrl.SIZE_EXPAND_FILL

	return _new_row(key, disp_text, value_ctrl, hint)


func _add_value(key, value, disp_text, hint=''):
	var value_ctrl = LineEdit.new()
	value_ctrl.size_flags_horizontal = value_ctrl.SIZE_EXPAND_FILL
	value_ctrl.text = value
	_wire_select_on_focus(value_ctrl)

	return _new_row(key, disp_text, value_ctrl, hint)


func _add_boolean(key, value, disp_text, hint=''):
	var value_ctrl = CheckBox.new()
	value_ctrl.button_pressed = value

	return _new_row(key, disp_text, value_ctrl, hint)


func _add_directory(key, value, disp_text, hint=''):
	var value_ctrl = DirectoryCtrl.new()
	value_ctrl.size_flags_horizontal = value_ctrl.SIZE_EXPAND_FILL
	value_ctrl.text = value
	_wire_select_on_focus(value_ctrl.get_line_edit())

	return _new_row(key, disp_text, value_ctrl, hint)


func _add_file(key, value, disp_text, hint=''):
	var value_ctrl = FileCtrl.new()
	value_ctrl.size_flags_horizontal = value_ctrl.SIZE_EXPAND_FILL
	value_ctrl.text = value
	_wire_select_on_focus(value_ctrl.get_line_edit())

	return _new_row(key, disp_text, value_ctrl, hint)

func _add_save_file_anywhere(key, value, disp_text, hint=''):
	var value_ctrl = SaveFileAnywhere.new()
	value_ctrl.size_flags_horizontal = value_ctrl.SIZE_EXPAND_FILL
	value_ctrl.text = value
	_wire_select_on_focus(value_ctrl.get_line_edit())

	return _new_row(key, disp_text, value_ctrl, hint)
	


func _add_color(key, value, disp_text, hint=''):
	var value_ctrl = ColorPickerButton.new()
	value_ctrl.size_flags_horizontal = value_ctrl.SIZE_EXPAND_FILL
	value_ctrl.color = value

	return _new_row(key, disp_text, value_ctrl, hint)


func _add_vector2(key, value, disp_text, hint=''):
	var value_ctrl = Vector2Ctrl.new()
	value_ctrl.size_flags_horizontal = value_ctrl.SIZE_EXPAND_FILL
	value_ctrl.value = value
	_wire_select_on_focus(value_ctrl.x_spin.get_line_edit())
	_wire_select_on_focus(value_ctrl.y_spin.get_line_edit())

	return _new_row(key, disp_text, value_ctrl, hint)
# -----------------------------


# ------------------
# Events
# ------------------
func _wire_select_on_focus(which):
	pass
	which.connect('focus_entered', _on_ctrl_focus_highlight.bind(which))
	which.connect('focus_exited', _on_ctrl_focus_unhighlight.bind(which))


func _on_ctrl_focus_highlight(which):
	if(which.has_method('select_all')):
		which.call_deferred('select_all')


func _on_ctrl_focus_unhighlight(which):
	if(which.has_method('select')):
		which.select(0, 0)


func _on_title_cell_draw(which):
	which.draw_rect(Rect2(Vector2(0, 0), which.size), Color(0, 0, 0, .15))


# ------------------
# Public
# ------------------
func get_config_issues():
	var to_return = []
	var has_directory = false

	for i in range(DIRS_TO_LIST):
		var key = str('directory_', i)
		var path = _cfg_ctrls[key].text
		if(path != null and path != ''):
			has_directory = true
			if(!DirAccess.dir_exists_absolute(path)):
				to_return.append(str('Test directory ', path, ' does not exist.'))

	if(!has_directory):
		to_return.append('You do not have any directories set.')

	if(!_cfg_ctrls['suffix'].text.ends_with('.gd')):
		to_return.append("Script suffix must end in '.gd'")

	return to_return


# --------------
# SUPER dumb but VERY fun hack to hide settings.  The various _add methods will
# return what they add.  If you want to hide it, just assign the result to this.
# YES, I could have just put .visible at the end, but I didn't think of that 
# until just now, and this was fun, non-permanent and the .visible at the end
# isn't as obvious as hide_this =
# 
# Also, we can't just skip adding the controls because other things are looking
# for them and things start to blow up if you don't add them.
var hide_this = null :
	set(val): 
		val.visible = false
# --------------
func set_options(options):
	_add_title("Settings")
	_add_number("log_level", options.log_level, "Log Level", 0, 3,
		"Detail level for log messages.\n" + \
		"\t0: Errors and failures only.\n" + \
		"\t1: Adds all test names + warnings + info\n" + \
		"\t2: Shows all asserts\n" + \
		"\t3: Adds more stuff probably, maybe not.")
	_add_boolean('ignore_pause', options.ignore_pause, 'Ignore Pause',
		"Ignore calls to pause_before_teardown")
	_add_boolean('hide_orphans', options.hide_orphans, 'Hide Orphans',
		'Do not display orphan counts in output.')
	_add_boolean('should_exit', options.should_exit, 'Exit on Finish',
		"Exit when tests finished.")
	_add_boolean('should_exit_on_success', options.should_exit_on_success, 'Exit on Success',
		"Exit if there are no failures.  Does nothing if 'Exit on Finish' is enabled.")


	_add_title("Panel Output")
	_add_select('output_font_name', options.panel_options.font_name, _avail_fonts, 'Font',
		"The name of the font to use when running tests and in the output panel to the left.")
	hide_this = _add_number('output_font_size', options.panel_options.font_size, 'Font Size', 5, 100,
		"The font size to use when running tests and in the output panel to the left.")


	_add_title('Runner Window')
	hide_this = _add_boolean("gut_on_top", options.gut_on_top, "On Top",
		"The GUT Runner appears above children added during tests.")
	_add_number('opacity', options.opacity, 'Opacity', 0, 100,
		"The opacity of GUT when tests are running.")
	hide_this = _add_boolean('should_maximize', options.should_maximize, 'Maximize',
		"Maximize GUT when tests are being run.")
	_add_boolean('compact_mode', options.compact_mode, 'Compact Mode',
		'The runner will be in compact mode.  This overrides Maximize.')

	_add_title('Runner Appearance')
	_add_select('font_name', options.font_name, _avail_fonts, 'Font',
		"The font to use for text output in the Gut Runner.")
	hide_this = _add_number('font_size', options.font_size, 'Font Size', 5, 100,
		"The font size for text output in the Gut Runner.")
	hide_this = _add_color('font_color', options.font_color, 'Font Color',
		"The font color for text output in the Gut Runner.")
	_add_color('background_color', options.background_color, 'Background Color',
		"The background color for text output in the Gut Runner.")
	_add_boolean('disable_colors', options.disable_colors, 'Disable Formatting',
		'Disable formatting and colors used in the Runner.  Does not affect panel output.')

	_add_title('Test Directories')
	_add_boolean('include_subdirs', options.include_subdirs, 'Include Subdirs',
		"Include subdirectories of the directories configured below.")
	for i in range(DIRS_TO_LIST):
		var value = ''
		if(options.dirs.size() > i):
			value = options.dirs[i]

		_add_directory(str('directory_', i), value, str('Directory ', i))

	_add_title("XML Output")
	_add_save_file_anywhere("junit_xml_file", options.junit_xml_file, "Output Path",
		"Path3D and filename where GUT should create a JUnit compliant XML file.  " +
		"This file will contain the results of the last test run.  To avoid " +
		"overriding the file use Include Timestamp.")
	_add_boolean("junit_xml_timestamp", options.junit_xml_timestamp, "Include Timestamp",
		"Include a timestamp in the filename so that each run gets its own xml file.")


	_add_title('Hooks')
	_add_file('pre_run_script', options.pre_run_script, 'Pre-Run Hook',
		'This script will be run by GUT before any tests are run.')
	_add_file('post_run_script', options.post_run_script, 'Post-Run Hook',
		'This script will be run by GUT after all tests are run.')


	_add_title('Misc')
	_add_value('prefix', options.prefix, 'Script Prefix',
		"The filename prefix for all test scripts.")
	_add_value('suffix', options.suffix, 'Script Suffix',
		"Script suffix, including .gd extension.  For example '_foo.gd'.")
	_add_number('paint_after', options.paint_after, 'Paint After', 0.0, 1.0,
		"How long GUT will wait before pausing for 1 frame to paint the screen.  0 is never.")
	# since _add_number doesn't set step property, it will default to a step of
	# 1 and cast values to int.  Give it a .5 step and re-set the value.
	_cfg_ctrls.paint_after.step = .05
	_cfg_ctrls.paint_after.value = options.paint_after

	print('GUT config loaded')

func get_options(base_opts):
	var to_return = base_opts.duplicate()

	# Settings
	to_return.log_level = _cfg_ctrls.log_level.value
	to_return.ignore_pause = _cfg_ctrls.ignore_pause.button_pressed
	to_return.hide_orphans = _cfg_ctrls.hide_orphans.button_pressed
	to_return.should_exit = _cfg_ctrls.should_exit.button_pressed
	to_return.should_exit_on_success = _cfg_ctrls.should_exit_on_success.button_pressed

	#Output
	to_return.panel_options.font_name = _cfg_ctrls.output_font_name.get_item_text(
		_cfg_ctrls.output_font_name.selected)
	to_return.panel_options.font_size = _cfg_ctrls.output_font_size.value

	# Runner Appearance
	to_return.font_name = _cfg_ctrls.font_name.get_item_text(
		_cfg_ctrls.font_name.selected)
	to_return.font_size = _cfg_ctrls.font_size.value
	to_return.should_maximize = _cfg_ctrls.should_maximize.button_pressed
	to_return.compact_mode = _cfg_ctrls.compact_mode.button_pressed
	to_return.opacity = _cfg_ctrls.opacity.value
	to_return.background_color = _cfg_ctrls.background_color.color.to_html()
	to_return.font_color = _cfg_ctrls.font_color.color.to_html()
	to_return.disable_colors = _cfg_ctrls.disable_colors.button_pressed
	to_return.gut_on_top = _cfg_ctrls.gut_on_top.button_pressed
	to_return.paint_after = _cfg_ctrls.paint_after.value


	# Directories
	to_return.include_subdirs = _cfg_ctrls.include_subdirs.button_pressed
	var dirs = []
	for i in range(DIRS_TO_LIST):
		var key = str('directory_', i)
		var val = _cfg_ctrls[key].text
		if(val != '' and val != null):
			dirs.append(val)
	to_return.dirs = dirs

	# XML Output
	to_return.junit_xml_file = _cfg_ctrls.junit_xml_file.text
	to_return.junit_xml_timestamp = _cfg_ctrls.junit_xml_timestamp.button_pressed

	# Hooks
	to_return.pre_run_script = _cfg_ctrls.pre_run_script.text
	to_return.post_run_script = _cfg_ctrls.post_run_script.text

	# Misc
	to_return.prefix = _cfg_ctrls.prefix.text
	to_return.suffix = _cfg_ctrls.suffix.text

	return to_return
