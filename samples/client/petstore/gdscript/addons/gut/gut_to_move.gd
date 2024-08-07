# Temporary base script for gut.gd to hold the things to be remvoed and added
# to some utility somewhere.
extends Node
var _utils = load('res://addons/gut/utils.gd').get_instance()

# ------------------------------------------------------------------------------
# deletes all files in a given directory
# ------------------------------------------------------------------------------
func directory_delete_files(path):
	var d = DirAccess.open(path)

	# SHORTCIRCUIT
	if(d == null):
		return

	# Traversing a directory is kinda odd.  You have to start the process of listing
	# the contents of a directory with list_dir_begin then use get_next until it
	# returns an empty string.  Then I guess you should end it.
	d.list_dir_begin() # TODOGODOT4 fill missing arguments https://github.com/godotengine/godot/pull/40547
	var thing = d.get_next() # could be a dir or a file or something else maybe?
	var full_path = ''
	while(thing != ''):
		full_path = path + "/" + thing
		#file_exists returns fasle for directories
		if(d.file_exists(full_path)):
			d.remove(full_path)
		thing = d.get_next()

	d.list_dir_end()

# ------------------------------------------------------------------------------
# deletes the file at the specified path
# ------------------------------------------------------------------------------
func file_delete(path):
	var d = DirAccess.open(path.get_base_dir())
	if(d != null):
		d.remove(path)

# ------------------------------------------------------------------------------
# Checks to see if the passed in file has any data in it.
# ------------------------------------------------------------------------------
func is_file_empty(path):
	var f = FileAccess.open(path, FileAccess.READ)
	var result = FileAccess.get_open_error()
	var empty = true
	if(result == OK):
		empty = f.get_length() == 0
	f = null
	return empty

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
func get_file_as_text(path):
	return _utils.get_file_as_text(path)

# ------------------------------------------------------------------------------
# Creates an empty file at the specified path
# ------------------------------------------------------------------------------
func file_touch(path):
	FileAccess.open(path, FileAccess.WRITE)

# ------------------------------------------------------------------------------
# Call _process or _fixed_process, if they exist, on obj and all it's children
# and their children and so and so forth.  Delta will be passed through to all
# the _process or _fixed_process methods.
# ------------------------------------------------------------------------------
func simulate(obj, times, delta):
	for _i in range(times):
		if(obj.has_method("_process")):
			obj._process(delta)
		if(obj.has_method("_physics_process")):
			obj._physics_process(delta)

		for kid in obj.get_children():
			simulate(kid, 1, delta)
