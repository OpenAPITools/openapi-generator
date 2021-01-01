require "kilt"

CONTENT_FOR_BLOCKS = Hash(String, Tuple(String, Proc(String))).new

# `content_for` is a set of helpers that allows you to capture
# blocks inside views to be rendered later during the request. The most
# common use is to populate different parts of your layout from your view.
#
# The currently supported engines are: ecr and slang.
#
# ## Usage
#
# You call `content_for`, generally from a view, to capture a block of markup
# giving it an identifier:
#
# ```
# # index.ecr
# <% content_for "some_key" do %>
#   <chunk of="html">...</chunk>
# <% end %>
# ```
#
# Then, you call `yield_content` with that identifier, generally from a
# layout, to render the captured block:
#
# ```
# # layout.ecr
# <%= yield_content "some_key" %>
# ```
#
# ## And How Is This Useful?
#
# For example, some of your views might need a few javascript tags and
# stylesheets, but you don't want to force this files in all your pages.
# Then you can put `<%= yield_content :scripts_and_styles %>` on your
# layout, inside the <head> tag, and each view can call `content_for`
# setting the appropriate set of tags that should be added to the layout.
macro content_for(key, file = __FILE__)
  %proc = ->() {
    __kilt_io__ = IO::Memory.new
    {{ yield }}
    __kilt_io__.to_s
  }

  CONTENT_FOR_BLOCKS[{{key}}] = Tuple.new {{file}}, %proc
  nil
end

# Yields content for the given key if a `content_for` block exists for that key.
macro yield_content(key)
  if CONTENT_FOR_BLOCKS.has_key?({{key}})
    __caller_filename__ = CONTENT_FOR_BLOCKS[{{key}}][0]
    %proc = CONTENT_FOR_BLOCKS[{{key}}][1]
    %proc.call if __content_filename__ == __caller_filename__
  end
end

# Render view with a layout as the superview.
#
# ```
# render "src/views/index.ecr", "src/views/layout.ecr"
# ```
macro render(filename, layout)
  __content_filename__ = {{filename}}
  content = render {{filename}}
  render {{layout}}
end

# Render view with the given filename.
macro render(filename)
  Kilt.render({{filename}})
end

# Halt execution with the current context.
# Returns 200 and an empty response by default.
#
# ```
# halt env, status_code: 403, response: "Forbidden"
# ```
macro halt(env, status_code = 200, response = "")
  {{env}}.response.status_code = {{status_code}}
  {{env}}.response.print {{response}}
  {{env}}.response.close
  next
end

# Extends context storage with user defined types.
#
# ```
# class User
#   property name
# end
#
# add_context_storage_type(User)
# ```
macro add_context_storage_type(type)
  {{ HTTP::Server::Context::STORE_MAPPINGS.push(type) }}
end
