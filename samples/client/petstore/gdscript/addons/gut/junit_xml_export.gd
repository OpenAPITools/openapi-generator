# ------------------------------------------------------------------------------
# Creates an export of a test run in the JUnit XML format.
# ------------------------------------------------------------------------------
var _utils = load('res://addons/gut/utils.gd').get_instance()

var _exporter = _utils.ResultExporter.new()

func indent(s, ind):
	var to_return = ind + s
	to_return = to_return.replace("\n", "\n" + ind)
	return to_return


func add_attr(name, value):
	return str(name, '="', value, '" ')

func _export_test_result(test):
	var to_return = ''

	# Right now the pending and failure messages won't fit in the message
	# attribute because they can span multiple lines and need to be escaped.
	if(test.status == 'pending'):
		var skip_tag = str("<skipped message=\"pending\">", test.pending[0], "</skipped>")
		to_return += skip_tag
	elif(test.status == 'fail'):
		var fail_tag = str("<failure message=\"failed\">", test.failing[0], "</failure>")
		to_return += fail_tag

	return to_return


func _export_tests(script_result, classname):
	var to_return = ""

	for key in script_result.keys():
		var test = script_result[key]
		var assert_count = test.passing.size() + test.failing.size()
		to_return += "<testcase "
		to_return += add_attr("name", key)
		to_return += add_attr("assertions", assert_count)
		to_return += add_attr("status", test.status)
		to_return += add_attr("classname", classname)
		to_return += ">\n"

		to_return += _export_test_result(test)

		to_return += "</testcase>\n"

	return to_return


func _export_scripts(exp_results):
	var to_return = ""
	for key in exp_results.test_scripts.scripts.keys():
		var s = exp_results.test_scripts.scripts[key]
		to_return += "<testsuite "
		to_return += add_attr("name", key)
		to_return += add_attr("tests", s.props.tests)
		to_return += add_attr("failures", s.props.failures)
		to_return += add_attr("skipped", s.props.pending)
		to_return += ">\n"

		to_return += indent(_export_tests(s.tests, key), "    ")

		to_return += "</testsuite>\n"

	return to_return


func get_results_xml(gut):
	var exp_results = _exporter.get_results_dictionary(gut)
	var to_return = '<?xml version="1.0" encoding="UTF-8"?>' + "\n"
	to_return += '<testsuites '
	to_return += add_attr("name", 'GutTests')
	to_return += add_attr("failures", exp_results.test_scripts.props.failures)
	to_return += add_attr('tests', exp_results.test_scripts.props.tests)
	to_return += ">\n"

	to_return += indent(_export_scripts(exp_results), "  ")

	to_return += '</testsuites>'
	return to_return


func write_file(gut, path):
	var xml = get_results_xml(gut)

	var f_result = _utils.write_file(path, xml)
	if(f_result != OK):
		var msg = str("Error:  ", f_result, ".  Could not create export file ", path)
		_utils.get_logger().error(msg)

	return f_result

