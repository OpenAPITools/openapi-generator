module Kemal
  module Utils
    ZIP_TYPES = {".htm", ".html", ".txt", ".css", ".js", ".svg", ".json", ".xml", ".otf", ".ttf", ".woff", ".woff2"}

    def self.path_starts_with_slash?(path : String)
      path.starts_with? '/'
    end

    def self.zip_types(path : String) # https://github.com/h5bp/server-configs-nginx/blob/master/nginx.conf
      ZIP_TYPES.includes? File.extname(path)
    end
  end
end
