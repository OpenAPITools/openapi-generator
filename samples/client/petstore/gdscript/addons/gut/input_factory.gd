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
# ##############################################################################


# Implemented InputEvent* convenience methods
# 	InputEventAction
# 	InputEventKey
# 	InputEventMouseButton
# 	InputEventMouseMotion

# Yet to implement InputEvents
# 	InputEventJoypadButton
# 	InputEventJoypadMotion
# 	InputEventMagnifyGesture
# 	InputEventMIDI
# 	InputEventPanGesture
# 	InputEventScreenDrag
# 	InputEventScreenTouch


static func _to_scancode(which):
	var key_code = which
	if(typeof(key_code) == TYPE_STRING):
		key_code = key_code.to_upper().to_ascii_buffer()[0]
	return key_code


static func new_mouse_button_event(position, global_position, pressed, button_index):
	var event = InputEventMouseButton.new()
	event.position = position
	if(global_position != null):
		event.global_position = global_position
	event.pressed = pressed
	event.button_index = button_index

	return event


static func key_up(which):
	var event = InputEventKey.new()
	event.keycode = _to_scancode(which)
	event.pressed = false
	return event


static func key_down(which):
	var event = InputEventKey.new()
	event.keycode = _to_scancode(which)
	event.pressed = true
	return event


static func action_up(which, strength=1.0):
	var event  = InputEventAction.new()
	event.action = which
	event.strength = strength
	return event


static func action_down(which, strength=1.0):
	var event  = InputEventAction.new()
	event.action = which
	event.strength = strength
	event.pressed = true
	return event


static func mouse_left_button_down(position, global_position=null):
	var event = new_mouse_button_event(position, global_position, true, MOUSE_BUTTON_LEFT)
	return event


static func mouse_left_button_up(position, global_position=null):
	var event = new_mouse_button_event(position, global_position, false, MOUSE_BUTTON_LEFT)
	return event


static func mouse_double_click(position, global_position=null):
	var event = new_mouse_button_event(position, global_position, false, MOUSE_BUTTON_LEFT)
	event.double_click = true
	return event


static func mouse_right_button_down(position, global_position=null):
	var event = new_mouse_button_event(position, global_position, true, MOUSE_BUTTON_RIGHT)
	return event


static func mouse_right_button_up(position, global_position=null):
	var event = new_mouse_button_event(position, global_position, false, MOUSE_BUTTON_RIGHT)
	return event


static func mouse_motion(position, global_position=null):
	var event = InputEventMouseMotion.new()
	event.position = position
	if(global_position != null):
		event.global_position = global_position
	return event


static func mouse_relative_motion(offset, last_motion_event=null, speed=Vector2(0, 0)):
	var event = null
	if(last_motion_event == null):
		event = mouse_motion(offset)
		event.velocity = speed
	else:
		event = last_motion_event.duplicate()
		event.position += offset
		event.global_position += offset
		event.relative = offset
		event.velocity = speed
	return event
