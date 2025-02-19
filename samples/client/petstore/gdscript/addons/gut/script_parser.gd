# ------------------------------------------------------------------------------
# List of methods that should not be overloaded when they are not defined
# in the class being doubled.  These either break things if they are
# overloaded or do not have a "super" equivalent so we can't just pass
# through.
const BLACKLIST = [
	'_draw',
	'_enter_tree',
	'_exit_tree',
	'_get_minimum_size', # Nonexistent function _get_minimum_size
	'_get', # probably
	'_input',
	'_notification',
	'_physics_process',
	'_process',
	'_set',
	'_to_string', # nonexistant function super._to_string
	'_unhandled_input',
	'_unhandled_key_input',
	'draw_mesh', # issue with one parameter, value is `Null((..), (..), (..))``
	'emit_signal', # can't handle extra parameters to be sent with signal.
	'get_path',
	'get_script',
	'get',
	'has_method',

	'print_orphan_nodes'
]


# ------------------------------------------------------------------------------
# Combins the meta for the method with additional information.
# * flag for whether the method is local
# * adds a 'default' property to all parameters that can be easily checked per
#   parameter
# ------------------------------------------------------------------------------
class ParsedMethod:
	var _meta = {}
	var meta = _meta :
		get: return _meta
		set(val): return;

	var _parameters = []
	var is_local = false

	const NO_DEFAULT = '__no__default__'

	func _init(metadata):
		_meta = metadata
		var start_default = _meta.args.size() - _meta.default_args.size()
		for i in range(_meta.args.size()):
			var arg = _meta.args[i]
			# Add a "default" property to the metadata so we don't have to do
			# weird default position math again.
			if(i >= start_default):
				arg['default'] = _meta.default_args[start_default - i]
			else:
				arg['default'] = NO_DEFAULT
			_parameters.append(arg)


	func is_black_listed():
		return BLACKLIST.find(_meta.name) != -1

	func is_accessor():
		return _meta.name.begins_with('@') and \
			(_meta.name.ends_with('_getter') or _meta.name.ends_with('_setter'))

	func to_s():
		var s = _meta.name + "("

		for i in range(_meta.args.size()):
			var arg = _meta.args[i]
			if(str(arg.default) != NO_DEFAULT):
				var val = str(arg.default)
				if(val == ''):
					val = '""'
				s += str(arg.name, ' = ', val)
			else:
				s += str(arg.name)

			if(i != _meta.args.size() -1):
				s += ', '

		s += ")"
		return s




# ------------------------------------------------------------------------------
# Doesn't know if a method is local and in super, but not sure if that will
# ever matter.
# ------------------------------------------------------------------------------
class ParsedScript:
	# All methods indexed by name.
	var _methods_by_name = {}
	var _utils = load('res://addons/gut/utils.gd').get_instance()

	var _script_path = null
	var script_path = _script_path :
		get: return _script_path
		set(val): return;

	var _subpath = null
	var subpath = null :
		get: return _subpath
		set(val): return;

	var _resource = null
	var resource = null :
		get: return _resource
		set(val): return;

	var _native_instance = null

	var is_native = false :
		get: return _native_instance != null
		set(val): return;

	func unreference():
		if(_native_instance != null):
			_native_instance.free()
		return super()


	func _init(script_or_inst, inner_class=null):
		var to_load = script_or_inst

		if(_utils.is_native_class(to_load)):
			_resource = to_load
			_native_instance = to_load.new()
		else:
			if(!script_or_inst is Resource):
				to_load = load(script_or_inst.get_script().get_path())

			_script_path = to_load.resource_path
			if(inner_class != null):
				_subpath = _find_subpath(to_load, inner_class)

			if(inner_class == null):
				_resource = to_load
			else:
				_resource = inner_class
				to_load = inner_class

		_parse_methods(to_load)


	func _has_flag_to_be_ignored(flags):
		return false
		# I think this is getting anything that has the 1 flag set...I think
		return 	flags & (1 << 2) == 0 && \
				flags & (1 << 4) == 0 && \
				flags & (1 << 6) == 0


	func _print_flags(meta):
		print(str(meta.name, ':').rpad(30), str(meta.flags).rpad(4), ' = ', _utils.dec2bistr(meta.flags, 10))


	func _get_native_methods(base_type):
		var to_return = []
		if(base_type != null):
			var source = str('extends ', base_type)
			var inst = _utils.create_script_from_source(source).new()
			to_return = inst.get_method_list()
			if(! inst is RefCounted):
				inst.free()
		return to_return


	func _parse_methods(thing):
		var methods = []
		if(is_native):
			methods = _native_instance.get_method_list()
		else:
			var base_type = thing.get_instance_base_type()
			methods = _get_native_methods(base_type)

		for m in methods:
			if(!_has_flag_to_be_ignored(m.flags)):
				var parsed = ParsedMethod.new(m)
				_methods_by_name[m.name] = parsed
				# _init must always be included so that we can initialize
				# double_tools
				if(m.name == '_init'):
					parsed.is_local = true


		# This loop will overwrite all entries in _methods_by_name with the local
		# method object so there is only ever one listing for a function with
		# the right "is_local" flag.
		if(!is_native):
			methods = thing.get_script_method_list()
			for m in methods:
				var parsed_method = ParsedMethod.new(m)
				parsed_method.is_local = true
				_methods_by_name[m.name] = parsed_method


	func _find_subpath(parent_script, inner):
		var const_map = parent_script.get_script_constant_map()
		var consts = const_map.keys()
		var const_idx = 0
		var found = false
		var to_return = null

		while(const_idx < consts.size() and !found):
			var key = consts[const_idx]
			var const_val = const_map[key]
			if(typeof(const_val) == TYPE_OBJECT):
				if(const_val == inner):
					found = true
					to_return = key
				else:
					to_return = _find_subpath(const_val, inner)
					if(to_return != null):
						to_return = str(key, '.', to_return)
						found = true

			const_idx += 1

		return to_return


	func get_method(name):
		return _methods_by_name[name]


	func is_method_blacklisted(m_name):
		if(_methods_by_name.has(m_name)):
			return _methods_by_name[m_name].is_black_listed()


	func get_super_method(name):
		var to_return = get_method(name)
		if(to_return.is_local):
			to_return = null

		return to_return

	func get_local_method(name):
		var to_return = get_method(name)
		if(!to_return.is_local):
			to_return = null

		return to_return


	func get_sorted_method_names():
		var keys = _methods_by_name.keys()
		keys.sort()
		return keys


	func get_local_method_names():
		var names = []
		for method in _methods_by_name:
			if(_methods_by_name[method].is_local):
				names.append(method)

		return names


	func get_super_method_names():
		var names = []
		for method in _methods_by_name:
			if(!_methods_by_name[method].is_local):
				names.append(method)

		return names


	func get_local_methods():
		var to_return = []
		for key in _methods_by_name:
			var method = _methods_by_name[key]
			if(method.is_local):
				to_return.append(method)
		return to_return


	func get_super_methods():
		var to_return = []
		for key in _methods_by_name:
			var method = _methods_by_name[key]
			if(!method.is_local):
				to_return.append(method)
		return to_return


	func get_extends_text():
		var text = null
		if(is_native):
			text = str("extends ", _native_instance.get_class())
		else:
			text = str("extends '", _script_path, "'")
			if(_subpath != null):
				text += '.' + _subpath
		return text


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
var scripts = {}
var _utils = load('res://addons/gut/utils.gd').get_instance()


func _get_instance_id(thing):
	var inst_id = null

	if(_utils.is_native_class(thing)):
		var id_str = str(thing).replace("<", '').replace(">", '').split('#')[1]
		inst_id = id_str.to_int()
	elif(typeof(thing) == TYPE_STRING):
		if(FileAccess.file_exists(thing)):
			inst_id = load(thing).get_instance_id()
	else:
		inst_id = thing.get_instance_id()

	return inst_id


func parse(thing, inner_thing=null):
	var key = -1
	if(inner_thing == null):
		key = _get_instance_id(thing)
	else:
		key = _get_instance_id(inner_thing)

	var parsed = null

	if(key != null):
		if(scripts.has(key)):
			parsed = scripts[key]
		else:
			var obj = instance_from_id(_get_instance_id(thing))
			var inner = null
			if(inner_thing != null):
				inner = instance_from_id(_get_instance_id(inner_thing))

			if(obj is Resource or _utils.is_native_class(obj)):
				parsed = ParsedScript.new(obj, inner)
				scripts[key] = parsed

	return parsed

