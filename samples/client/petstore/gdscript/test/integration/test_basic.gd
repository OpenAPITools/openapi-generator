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


func fail_test(msg:=""):
	super.fail_test(msg)
	emit_signal("test_ended")


func test_authenticated_user_uses_monkey_crud():
	
	var someone := DemoUserModel.new()
	someone.username = "Anon"
	someone.password = "nonA"
	
	var user_api := DemoUserApi.new(cfg)
	user_api.create_user(
		someone,
		func(response):
			prints("Created user %s:" % someone.username, response)
			assert_is(response, DemoApiResponse)
			# The response holds {"code":200,"type":"unknown","message":"<new user id>"}
			# How can the client know how to deserialize a DemoUserModel from that ?
			#assert_is(response.data, DemoUserModel)
			#assert_eq(response.data.username, someone.username)
			authenticate(
				someone.username, someone.password,
				func():
					add_monkey(
						func(monkey):
							update_monkey(
								monkey, "Summer",
								func(_updated_monkey):
									delete_monkey(
										monkey.id,
										func(_done):
											emit_signal("test_ended")
									)
							)
					)
			)
			,
		func(error):
			fail_test(str(error))
	)
	
	await wait_for_signal(test_ended, 120, "Waiting for test end…")


func authenticate(username: String, password: String, on_done: Callable):
	var user_api := DemoUserApi.new(cfg)
	user_api.login_user(
		username, password,
		func(response):
			prints("Login Response:", response)
			assert_is(response, DemoApiResponse)
			assert_eq(response.code, 200)
			on_done.call()
			,
		func(error):
			fail_test(str(error))
			,
	)


func add_monkey(on_done: Callable):
	
	var monkey := DemoPetModel.new()
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
		func(response):
			prints("Added monkey:", response)
			assert_is(response, DemoApiResponse)
			assert_eq(response.code, 200)
			assert_is(response.data, DemoPetModel)
			assert_eq(response.data.name, monkey.name)
			
			on_done.call(response.data)
			,
		func(error: DemoApiError):
			assert_is(error, DemoApiError)
			fail_test(str(error))
			,
	)


func update_monkey(monkey, new_name, on_done: Callable):

	var pet_api := DemoPetApi.new(cfg)
	pet_api.update_pet_with_form(
		monkey.id, new_name, "available",
		func(response):
			prints("Updated monkey:", response)
			assert_eq(response.code, 200)
			on_done.call(response.data)
			,
		func(error):
			fail_test(str(error))
			,
	)


func delete_monkey(monkey_id, on_done: Callable):

	var pet_api := DemoPetApi.new(cfg)
	pet_api.delete_pet(
		monkey_id, "whyisapikeyhere",
		func(response):
			prints("Deleted monkey:", response)
			assert_eq(response.code, 200)
			on_done.call(response)
			,
		func(error):
			fail_test(str(error))
			,
	)
