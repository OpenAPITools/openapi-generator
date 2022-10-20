extends Control


# Note: we also log to /tmp/gdscript_petstore.log
# And since I can't figure out how to set an exit code,
# we will need to parse that log (read its last line) to assess
# whether the test suite passed or not. (when integrating with CI)
const LAST_LINE_SUITE_FAILED := "ERROR"
const LAST_LINE_SUITE_PASSED := "OK"


@onready var log_text_edit := $HBoxContainer/LogDump


var failed := false


func _ready():
	$HBoxContainer/VBoxContainer/RunTestsButton.grab_focus()
	Logger.text_edit_to_fill = log_text_edit
	Logger.inform("Petstore demo is ready.")
	if is_headless():
		run_all_tests()
		#gtfo()


func _on_run_tests_button_pressed():
	run_all_tests()


func _on_exit_button_pressed():
	gtfo()


func gtfo(err_code := 0):
	Engine.get_main_loop().quit(err_code)


func is_headless() -> bool:
	# I have no shame, and no idea how to detect --headless
	#print(OS.get_cmdline_args())  # empty
	#print(OS.get_cmdline_user_args())  # empty
	#print(OS.get_environment("HEADLESS"))  # works, but cumbersome
	return "" == RenderingServer.get_video_adapter_name()


func fail(msg: String):
	Logger.error(msg)
	Logger.inform(LAST_LINE_SUITE_FAILED)
	failed = true
	gtfo(1)


func run_all_tests():
	log_text_edit.text = ""
	run_test_01()
	
	# Oh no that won't work ; async woes
	if not failed:
		Logger.inform(LAST_LINE_SUITE_PASSED)


func run_test_01():
	Logger.inform("Running test 01…")
	
	var monkey = Pet.new()
	monkey.name = "Grégoire"
	monkey.tags = ['tree', 'fur']
	
	var pet_api := PetApi.new()
	pet_api.bee_port = 81
	pet_api.add_pet(
		monkey,
		func(result):
			print("Added monkey.")
			,
		func(error):
			Logger.warn("OH NOES!")
			Logger.warn(str(error))
			,
	)
	
	
	

