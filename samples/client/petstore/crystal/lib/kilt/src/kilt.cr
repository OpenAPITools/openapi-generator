require "./kilt/version"
require "./kilt/exception"

module Kilt
  # macro only constant
  ENGINES = {} of String => Int32

  macro register_engine(ext, embed_macro)
    {% Kilt::ENGINES[ext] = embed_macro.id %}
  end

  macro embed(filename, io_name = "__kilt_io__", *args)
    {% ext = filename.split(".").last %}

    {% if Kilt::ENGINES[ext] %}
      {{Kilt::ENGINES[ext]}}({{filename}}, {{io_name}}, {{*args}})
    {% else %}
      raise Kilt::Exception.new("Unsupported template engine for extension: \"" + {{ext}} + "\"")
    {% end %}
  end

  macro render(filename, *args)
    String.build do |__kilt_io__|
      Kilt.embed({{filename}}, "__kilt_io__", {{*args}})
    end
  end

  macro file(filename, io_name = "__kilt_io__", *args)
    def to_s({{io_name.id}})
      Kilt.embed({{filename}}, {{io_name}}, {{*args}})
    end
  end
end

require "./ecr"
