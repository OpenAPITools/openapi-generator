extends Control

# This is the main (and only) scene of this project.
# It runs GUT, whose tests are defined in `test/`.
# This project is meant to be run in headless mode:
#   godot --debug --headless --path samples/client/petstore/gdscript --script addons/gut/gut_cmdln.gd

signal test_ended

@onready var run_tests_button := $HBoxContainer/VBoxContainer/RunTestsButton

var gut


func _ready():
	await get_tree().get_root().ready
	
	if is_headless():
		run_all_tests(func(): gtfo())
	else:
		run_tests_button.grab_focus()


func _on_run_tests_button_pressed():
	run_tests_button.disabled = true
	run_all_tests(
		func():
			run_tests_button.disabled = false
	)


func _on_exit_button_pressed():
	gtfo()


func gtfo(err_code := 0):
	Engine.get_main_loop().quit(err_code)


func is_headless() -> bool:
	# I have no shame, and no other idea how to detect --headless
	#print(OS.get_cmdline_args())  # empty
	#print(OS.get_cmdline_user_args())  # empty
	#print(OS.get_environment("HEADLESS"))  # would work, but cumbersome
	return "" == RenderingServer.get_video_adapter_name()


func run_all_tests(on_done: Callable):
	print("Running all tests…")
	
	if self.gut:
		self.gut.queue_free()
	
	self.gut = load("res://addons/gut/gui/GutRunner.tscn").instantiate()
	self.gut.set_name("GutRunner")
	add_child(self.gut)
	
	await self.gut.get_gut().end_run
	on_done.call()


