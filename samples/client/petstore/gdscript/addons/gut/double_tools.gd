var thepath = ''
var subpath = ''
var stubber = null
var spy = null
var gut = null
var from_singleton = null
var is_partial = null
var double = null

const NO_DEFAULT_VALUE = '!__gut__no__default__value__!'
func from_id(inst_id):
	if(inst_id ==  -1):
		return null
	else:
		return instance_from_id(inst_id)

func should_call_super(method_name, called_with):
	if(stubber != null):
		return stubber.should_call_super(double, method_name, called_with)
	else:
		return false

func spy_on(method_name, called_with):
	if(spy != null):
		spy.add_call(double, method_name, called_with)

func get_stubbed_return(method_name, called_with):
	if(stubber != null):
		return stubber.get_return(double, method_name, called_with)
	else:
		return null

func default_val(method_name, p_index, default_val=NO_DEFAULT_VALUE):
	if(stubber != null):
		return stubber.get_default_value(double, method_name, p_index)
	else:
		return null

func _init(values=null):
	if(values != null):
		double = values.double
		thepath = values.thepath
		subpath = values.subpath
		stubber = from_id(values.stubber)
		spy = from_id(values.spy)
		gut = from_id(values.gut)
		from_singleton = values.from_singleton
		is_partial = values.is_partial

	if(gut != null):
		gut.get_autofree().add_free(double)

