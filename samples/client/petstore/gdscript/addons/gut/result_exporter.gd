# ------------------------------------------------------------------------------
# Creates a structure that contains all the data about the results of running
# tests.  This was created to make an intermediate step organizing the result
# of a run and exporting it in a specific format.  This can also serve as a
# unofficial GUT export format.
# ------------------------------------------------------------------------------
var _utils = load('res://addons/gut/utils.gd').get_instance()
var json = JSON.new()

func _export_tests(summary_script):
	var to_return = {}
	var tests = summary_script.get_tests()
	for key in tests.keys():
		to_return[key] = {
			"status":tests[key].get_status(),
			"passing":tests[key].pass_texts,
			"failing":tests[key].fail_texts,
			"pending":tests[key].pending_texts,
			"orphans":tests[key].orphans
		}

	return to_return

# TODO
#	errors
func _export_scripts(summary):
	if(summary == null):
		return {}

	var scripts = {}

	for s in summary.get_scripts():
		scripts[s.name] = {
			'props':{
				"tests":s._tests.size(),
				"pending":s.get_pending_count(),
				"failures":s.get_fail_count(),
			},
			"tests":_export_tests(s)
		}
	return scripts

func _make_results_dict():
	var result =  {
		'test_scripts':{
			"props":{
				"pending":0,
				"failures":0,
				"passing":0,
				"tests":0,
				"time":0,
				"orphans":0,
				"errors":0,
				"warnings":0
			},
			"scripts":[]
		}
	}
	return result


# TODO
#	time
#	errors
func get_results_dictionary(gut, include_scripts=true):
	var summary = gut.get_summary()
	var scripts = []

	if(include_scripts):
		scripts = _export_scripts(summary)

	var result =  _make_results_dict()
	if(summary != null):
		var totals = summary.get_totals()

		var props = result.test_scripts.props
		props.pending = totals.pending
		props.failures = totals.failing
		props.passing = totals.passing_tests
		props.tests = totals.tests
		props.errors = gut.logger.get_errors().size()
		props.warnings = gut.logger.get_warnings().size()
		props.time =  gut.get_elapsed_time()
		props.orphans = gut.get_orphan_counter().get_counter('total')
		result.test_scripts.scripts = scripts

	return result


func write_json_file(gut, path):
	var dict = get_results_dictionary(gut)
	var json_text = json.stringify(dict, ' ')

	var f_result = _utils.write_file(path, json_text)
	if(f_result != OK):
		var msg = str("Error:  ", f_result, ".  Could not create export file ", path)
		_utils.get_logger().error(msg)

	return f_result



func write_summary_file(gut, path):
	var dict = get_results_dictionary(gut, false)
	var json_text = json.stringify(dict, ' ')

	var f_result = _utils.write_file(path, json_text)
	if(f_result != OK):
		var msg = str("Error:  ", f_result, ".  Could not create export file ", path)
		_utils.get_logger().error(msg)

	return f_result
