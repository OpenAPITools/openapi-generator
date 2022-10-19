extends Control


func _ready():
	Logger.text_edit_to_fill = $HBoxContainer/LogDump
	Logger.inform("Petstore demo is ready.")


func _on_exit_button_pressed():
	Logger.inform("Goodbye.")
	get_tree().quit()


func _on_run_tests_button_pressed():
	pass # Replace with function body.
