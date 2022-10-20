extends Control

signal test_ended

@onready var run_tests_button := $HBoxContainer/VBoxContainer/RunTestsButton
@onready var log_text_edit := $HBoxContainer/LogDump


var failed := false


func _ready():
	# Note: we also log to /tmp/gdscript_petstore.log
	Logger.text_edit_to_fill = log_text_edit
	Logger.inform("Petstore demo is ready.")
	
	if is_headless():
		run_all_tests(func(): gtfo())
	else:
		run_tests_button.grab_focus()


func _on_run_tests_button_pressed():
	run_all_tests(func(): pass)


func _on_exit_button_pressed():
	gtfo()


func gtfo(err_code := 0):
	Engine.get_main_loop().quit(err_code)


func is_headless() -> bool:
	# I have no shame, and no other idea how to detect --headless
	#print(OS.get_cmdline_args())  # empty
	#print(OS.get_cmdline_user_args())  # empty
	#print(OS.get_environment("HEADLESS"))  # works, but cumbersome
	return "" == RenderingServer.get_video_adapter_name()


func fail(msg: String):
	Logger.error(msg)
	failed = true
	gtfo(1)


func run_all_tests(on_done := Callable()):
	log_text_edit.text = ""
	var started_at := Time.get_ticks_msec()
	run_test_01()
	await test_ended
#	run_test_02()
#	await test_ended
	# …
	var ended_at := Time.get_ticks_msec()
	Logger.inform("Ran tests for %.2fs" % [0.001 * (ended_at - started_at)])
	on_done.call()


func run_test_01():
	Logger.inform("Running test 01…")
	
	var monkey := Pet.new()
	monkey.name = "Grégoire"
	monkey.tags = ['tree', 'fur']
	
	var pet_api := PetApi.new()
	pet_api.bee_port = 81
	pet_api.add_pet(
		monkey,
		func(result):
			print("Added monkey.")
			emit_signal("test_ended")
			,
		#func(error: ApiError):  #  ←  straight up crash, try again later
		func(error):
			# OH GOSH THIS CRASHES AS WELL (works with RefCounted)
			# (but error does have type ApiError)
#			if not (error is ApiError):
#				fail("Error in on_failure callback has the wrong type.")
			printerr("ERROR!")
			fail(str(error))
			emit_signal("test_ended")
			,
	)
	
	
	

