@tool
extends EditorPlugin

var _bottom_panel = null


func _enter_tree():
	_bottom_panel = preload('res://addons/gut/gui/GutBottomPanel.tscn').instantiate()

	var button = add_control_to_bottom_panel(_bottom_panel, 'GUT')
	button.shortcut_in_tooltip = true

	await get_tree().create_timer(3).timeout
	_bottom_panel.set_interface(get_editor_interface())
	_bottom_panel.set_plugin(self)
	_bottom_panel.set_panel_button(button)
	_bottom_panel.load_shortcuts()


func _exit_tree():
	# Clean-up of the plugin goes here
	# Always remember to remove_at it from the engine when deactivated
	remove_control_from_bottom_panel(_bottom_panel)
	_bottom_panel.free()
