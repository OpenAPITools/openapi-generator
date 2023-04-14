extends GutTest

signal test_ended


var cfg := DemoApiConfig.new()


func configure():
	# We could perhaps load config from file and reuse the config across test files
	# or extend GutTest…  We'll see.
	#cfg.port = 80
	cfg.headers_base['api_key'] = "special-key"


func before_all():
	gut.p("Running basic tests…")
	configure()


func fail_test(msg=""):
	super.fail_test(msg)
	emit_signal("test_ended")


func test_authenticated_user_uses_monkey_crud():
	#gut.p("Running test 01…")
	
	var rick := DemoUser.new()
	rick.username = "Rick"
	rick.password = "ytrom&"
	
	var user_api := DemoUserApi.new(cfg)
	user_api.create_user(
		rick,
		func(result):
			prints("Created user %s." % rick.username, result)
			authenticate(
				rick.username, rick.password,
				func():
					add_monkey(
						func(monkey):
							update_monkey(
								monkey, "Summer",
								func(_result):
									emit_signal("test_ended")
							)
					)
			)
			,
		func(error):
			gut.p("ERROR!")
			fail_test(str(error))
			#emit_signal("test_ended")
	)
	
	await wait_for_signal(test_ended, 120, "Waiting for test end…")


func authenticate(username: String, password: String, on_done: Callable):
	var user_api := DemoUserApi.new(cfg)
	user_api.login_user(
		username, password,
		func(result):
			prints("Login Response:", result)
#			assert_eq(result.code, 200)
			on_done.call()
			,
		func(error):
			fail_test(str(error))
			,
	)


func add_monkey(on_done: Callable):
	
	var monkey := DemoPet.new()
	monkey.name = "Gregoire"
	monkey.photoUrls = ['urlA', 'urlB']
	monkey.status = "available"
	monkey.status = "shenaniganing"  # should raise a warning and not write
	assert_eq(monkey.status, "available")
	gut.p("NOTE: ERROR and WARNINGs above (about status) are expected and OK.")
	#monkey.tags = ['tree', 'fur']

	var pet_api := DemoPetApi.new(cfg)
	pet_api.add_pet(
		monkey,
		func(result, code, headers):
			print("Added monkey:", result)
			assert_eq(code, 200)
			on_done.call(result)
			,
		#func(error: ApiError):  #  ←  straight up crash, try again later
		func(error):
			# OH GOSH THIS CRASHES AS WELL (works with RefCounted)
			# (but error does have type ApiError)
#			if not (error is ApiError):
#				fail("Error in on_failure callback has the wrong type.")
			printerr("ERROR!")
			fail_test(str(error))
			,
	)


func update_monkey(monkey, new_name, on_done: Callable):

	var pet_api := DemoPetApi.new(cfg)
	pet_api.update_pet_with_form(
		monkey.id, new_name, "available",
		func(result):
			prints("Updated monkey:", result)
#			assert_eq(result.code, 200)
			on_done.call(result)
			,
		#func(error: ApiError):  #  ←  straight up crash, try again later
		func(error):
			# OH GOSH THIS CRASHES AS WELL (works with RefCounted)
			# (but error does have type ApiError)
#			if not (error is ApiError):
#				fail("Error in on_failure callback has the wrong type.")
			printerr("ERROR!")
			fail_test(str(error))
			,
	)
