require "kemal"

# Set root. If not specified the default content_type is 'text'
get "/" do
  "Hello Kemal!"
end

Kemal.run
