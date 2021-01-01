require "./kilt"
require "temel"

macro embed_temel(filename, __kilt_io__)
  __kilt_io__ << {{ run("./kilt/helpers/temel_embedder.cr", filename) }}
  __kilt_io__
end

Kilt.register_engine "temel", embed_temel
