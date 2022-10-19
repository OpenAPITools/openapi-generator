extends Node

# Singleton helper to log to console and some text edit.

enum LOG_LEVEL {
	DEBUG,  # → see all
	INFO,
	WARN,
	ERROR,
	SILENT,  # → see none
}

var log_level := LOG_LEVEL.DEBUG
var text_edit_to_fill: TextEdit

func __print(msg: String):
	print(msg)
	if text_edit_to_fill:
		text_edit_to_fill.text += "%s\n" % msg

func __printerr(msg: String):
	printerr(msg)
	if text_edit_to_fill:
		text_edit_to_fill.text += "\nERROR: %s\n\n" % msg

func debug(msg: String):
	if self.log_level == LOG_LEVEL.DEBUG:
		__print(msg)

func info(msg: String):
	inform(msg)

func inform(msg: String):
	if self.log_level == LOG_LEVEL.DEBUG || self.log_level == LOG_LEVEL.INFO:
		__print(msg)

func warn(msg: String):
	if self.log_level != LOG_LEVEL.ERROR && self.log_level != LOG_LEVEL.SILENT:
		__printerr(msg)

func error(msg: String):
	shout(msg)

func shout(msg: String):
	if self.log_level != LOG_LEVEL.SILENT:
		__printerr(msg)
