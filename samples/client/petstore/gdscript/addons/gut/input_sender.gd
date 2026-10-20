# ##############################################################################
#(G)odot (U)nit (T)est class
#
# ##############################################################################
# The MIT License (MIT)
# =====================
#
# Copyright (c) 2020 Tom "Butch" Wesley
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
# ##############################################################################
# Description
# -----------
# This class sends input to one or more recievers.  The receivers' _input,
# _unhandled_input, and _gui_input are called sending InputEvent* events.
# InputEvents can be sent via the helper methods or a custom made InputEvent
# can be sent via send_event(...)
#
# ##############################################################################
#extends "res://addons/gut/input_factory.gd"

# Implemented InputEvent* convenience methods
# 	InputEventAction
# 	InputEventKey
# 	InputEventMouseButton
#	InputEventMouseMotion

# Yet to implement InputEvents
# 	InputEventJoypadButton
# 	InputEventJoypadMotion
# 	InputEventMagnifyGesture
# 	InputEventMIDI
# 	InputEventPanGesture
# 	InputEventScreenDrag
# 	InputEventScreenTouch

class InputQueueItem:
	extends Node

	var events = []
	var time_delay = null
	var frame_delay = null
	var _waited_frames = 0
	var _is_ready = false
	var _delay_started = false

	signal event_ready

	# TODO should this be done in _physics_process instead or should it be
	# configurable?
	func _physics_process(delta):
		if(frame_delay > 0 and _delay_started):
			_waited_frames += 1
			if(_waited_frames >= frame_delay):
				emit_signal("event_ready")

	func _init(t_delay,f_delay):
		time_delay = t_delay
		frame_delay = f_delay
		_is_ready = time_delay == 0 and frame_delay == 0

	func _on_time_timeout():
		_is_ready = true
		emit_signal("event_ready")

	func _delay_timer(t):
		return Engine.get_main_loop().root.get_tree().create_timer(t)

	func is_ready():
		return _is_ready

	func start():
		_delay_started = true
		if(time_delay > 0):
			var t = _delay_timer(time_delay)
			t.connect("timeout",Callable(self,"_on_time_timeout"))


# ##############################################################################
#
# ##############################################################################
var _utils = load('res://addons/gut/utils.gd').get_instance()
var InputFactory = load("res://addons/gut/input_factory.gd")

const INPUT_WARN = 'If using Input as a reciever it will not respond to *_down events until a *_up event is recieved.  Call the appropriate *_up event or use hold_for(...) to automatically release after some duration.'

var _lgr = _utils.get_logger()
var _receivers = []
var _input_queue = []
var _next_queue_item = null
# used by mouse_relative_motion.  These use this instead of _last_event since
# it is logical to have a series of events happen between mouse motions.
var _last_mouse_motion = null
# used by hold_for and echo.
var _last_event = null

# indexed by keycode, each entry contains a boolean value indicating the
# last emitted "pressed" value for that keycode.
var _pressed_keys = {}
var _pressed_actions = {}
var _pressed_mouse_buttons = {}

var _auto_flush_input = false

signal idle


func _init(r=null):
	if(r != null):
		add_receiver(r)


func _send_event(event):
	if(event is InputEventKey):
		if((event.pressed and !event.echo) and is_key_pressed(event.keycode)):
			_lgr.warn(str("InputSender:  key_down called for ", event.as_text(), " when that key is already pressed.  ", INPUT_WARN))
		_pressed_keys[event.keycode] = event.pressed
	elif(event is InputEventAction):
		if(event.pressed and is_action_pressed(event.action)):
			_lgr.warn(str("InputSender:  action_down called for ", event.action, " when that action is already pressed.  ", INPUT_WARN))
		_pressed_actions[event.action] = event.pressed
	elif(event is InputEventMouseButton):
		if(event.pressed and is_mouse_button_pressed(event.button_index)):
			_lgr.warn(str("InputSender:  mouse_button_down called for ", event.button_index, " when that mouse button is already pressed.  ", INPUT_WARN))
		_pressed_mouse_buttons[event.button_index] = event

	for r in _receivers:
		if(r == Input):
			Input.parse_input_event(event)
			if(_auto_flush_input):
				Input.flush_buffered_events()
		else:
			if(r.has_method("_input")):
				r._input(event)

			if(r.has_method("_gui_input")):
				r._gui_input(event)

			if(r.has_method("_unhandled_input")):
				r._unhandled_input(event)


func _send_or_record_event(event):
	_last_event = event
	if(_next_queue_item != null):
		_next_queue_item.events.append(event)
	else:
		_send_event(event)


func _on_queue_item_ready(item):
	for event in item.events:
		_send_event(event)

	var done_event = _input_queue.pop_front()
	done_event.queue_free()

	if(_input_queue.size() == 0):
		_next_queue_item = null
		emit_signal("idle")
	else:
		_input_queue[0].start()


func _add_queue_item(item):
	item.connect("event_ready", _on_queue_item_ready.bind(item))
	_next_queue_item = item
	_input_queue.append(item)
	Engine.get_main_loop().root.add_child(item)
	if(_input_queue.size() == 1):
		item.start()


func add_receiver(obj):
	_receivers.append(obj)


func get_receivers():
	return _receivers


func wait(t):
	if(typeof(t) == TYPE_STRING):
		var suffix = t.substr(t.length() -1, 1)
		var val = t.rstrip('s').rstrip('f').to_float()

		if(suffix.to_lower() == 's'):
			wait_secs(val)
		elif(suffix.to_lower() == 'f'):
			wait_frames(val)
	else:
		wait_secs(t)

	return self


func wait_frames(num_frames):
	var item = InputQueueItem.new(0, num_frames)
	_add_queue_item(item)
	return self


func wait_secs(num_secs):
	var item = InputQueueItem.new(num_secs, 0)
	_add_queue_item(item)
	return self


# ------------------------------
# Event methods
# ------------------------------
func key_up(which):
	var event = InputFactory.key_up(which)
	_send_or_record_event(event)
	return self


func key_down(which):
	var event = InputFactory.key_down(which)
	_send_or_record_event(event)
	return self


func key_echo():
	if(_last_event != null and _last_event is InputEventKey):
		var new_key = _last_event.duplicate()
		new_key.echo = true
		_send_or_record_event(new_key)
	return self


func action_up(which, strength=1.0):
	var event  = InputFactory.action_up(which, strength)
	_send_or_record_event(event)
	return self


func action_down(which, strength=1.0):
	var event  = InputFactory.action_down(which, strength)
	_send_or_record_event(event)
	return self


func mouse_left_button_down(position, global_position=null):
	var event = InputFactory.mouse_left_button_down(position, global_position)
	_send_or_record_event(event)
	return self


func mouse_left_button_up(position, global_position=null):
	var event = InputFactory.mouse_left_button_up(position, global_position)
	_send_or_record_event(event)
	return self


func mouse_double_click(position, global_position=null):
	var event = InputFactory.mouse_double_click(position, global_position)
	event.double_click = true
	_send_or_record_event(event)
	return self


func mouse_right_button_down(position, global_position=null):
	var event = InputFactory.mouse_right_button_down(position, global_position)
	_send_or_record_event(event)
	return self


func mouse_right_button_up(position, global_position=null):
	var event = InputFactory.mouse_right_button_up(position, global_position)
	_send_or_record_event(event)
	return self


func mouse_motion(position, global_position=null):
	var event = InputFactory.mouse_motion(position, global_position)
	_last_mouse_motion = event
	_send_or_record_event(event)
	return self


func mouse_relative_motion(offset, speed=Vector2(0, 0)):
	var event = InputFactory.mouse_relative_motion(offset, _last_mouse_motion, speed)
	_last_mouse_motion = event
	_send_or_record_event(event)
	return self


func mouse_set_position(position, global_position=null):
	_last_mouse_motion = InputFactory.mouse_motion(position, global_position)
	return self


func send_event(event):
	_send_or_record_event(event)
	return self


func release_all():
	for key in _pressed_keys:
		if(_pressed_keys[key]):
			_send_event(InputFactory.key_up(key))
	_pressed_keys.clear()

	for key in _pressed_actions:
		if(_pressed_actions[key]):
			_send_event(InputFactory.action_up(key))
	_pressed_actions.clear()

	for key in _pressed_mouse_buttons:
		var event = _pressed_mouse_buttons[key].duplicate()
		if(event.pressed):
			event.pressed = false
			_send_event(event)
	_pressed_mouse_buttons.clear()


func hold_for(duration):
	if(_last_event != null and _last_event.pressed):
		var next_event = _last_event.duplicate()
		next_event.pressed = false
		wait(duration)
		send_event(next_event)
	return self


func clear():
	pass

	_last_event = null
	_last_mouse_motion = null
	_next_queue_item = null

	for item in _input_queue:
		item.free()
	_input_queue.clear()

	_pressed_keys.clear()
	_pressed_actions.clear()
	_pressed_mouse_buttons.clear()

func is_idle():
	return _input_queue.size() == 0

func is_key_pressed(which):
	var event = InputFactory.key_up(which)
	return _pressed_keys.has(event.keycode) and _pressed_keys[event.keycode]

func is_action_pressed(which):
	return _pressed_actions.has(which) and _pressed_actions[which]

func is_mouse_button_pressed(which):
	return _pressed_mouse_buttons.has(which) and _pressed_mouse_buttons[which]


func get_auto_flush_input():
	return _auto_flush_input


func set_auto_flush_input(val):
	_auto_flush_input = val