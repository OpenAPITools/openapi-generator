@tool
extends Window

@onready var _ctrls = {
	run_all = $Layout/CRunAll/ShortcutButton,
	run_current_script = $Layout/CRunCurrentScript/ShortcutButton,
	run_current_inner = $Layout/CRunCurrentInner/ShortcutButton,
	run_current_test = $Layout/CRunCurrentTest/ShortcutButton,
	panel_button = $Layout/CPanelButton/ShortcutButton,
}

func _ready():
	for key in _ctrls:
		var sc_button = _ctrls[key]
		sc_button.connect('start_edit', _on_edit_start.bind(sc_button))
		sc_button.connect('end_edit', _on_edit_end)


	# show dialog when running scene from editor.
	if(get_parent() == get_tree().root):
		popup_centered()

# ------------
# Events
# ------------
func _on_Hide_pressed():
	hide()

func _on_edit_start(which):
	for key in _ctrls:
		var sc_button = _ctrls[key]
		if(sc_button != which):
			sc_button.disable_set(true)
			sc_button.disable_clear(true)

func _on_edit_end():
	for key in _ctrls:
		var sc_button = _ctrls[key]
		sc_button.disable_set(false)
		sc_button.disable_clear(false)

# ------------
# Public
# ------------
func get_run_all():
	return _ctrls.run_all.get_shortcut()

func get_run_current_script():
	return _ctrls.run_current_script.get_shortcut()

func get_run_current_inner():
	return _ctrls.run_current_inner.get_shortcut()

func get_run_current_test():
	return _ctrls.run_current_test.get_shortcut()

func get_panel_button():
	return _ctrls.panel_button.get_shortcut()


func save_shortcuts(path):
	var f = ConfigFile.new()

	f.set_value('main', 'run_all', _ctrls.run_all.get_shortcut())
	f.set_value('main', 'run_current_script', _ctrls.run_current_script.get_shortcut())
	f.set_value('main', 'run_current_inner', _ctrls.run_current_inner.get_shortcut())
	f.set_value('main', 'run_current_test', _ctrls.run_current_test.get_shortcut())
	f.set_value('main', 'panel_button', _ctrls.panel_button.get_shortcut())

	f.save(path)


func load_shortcuts(path):
	var emptyShortcut = Shortcut.new()
	var f = ConfigFile.new()
	f.load(path)

	_ctrls.run_all.set_shortcut(f.get_value('main', 'run_all', emptyShortcut))
	_ctrls.run_current_script.set_shortcut(f.get_value('main', 'run_current_script', emptyShortcut))
	_ctrls.run_current_inner.set_shortcut(f.get_value('main', 'run_current_inner', emptyShortcut))
	_ctrls.run_current_test.set_shortcut(f.get_value('main', 'run_current_test', emptyShortcut))
	_ctrls.panel_button.set_shortcut(f.get_value('main', 'panel_button', emptyShortcut))
