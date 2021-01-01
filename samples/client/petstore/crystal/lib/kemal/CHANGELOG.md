# 0.27.0 (28-11-2020)

- Crystal 0.35.x support :tada: Thanks @bcardiff :pray:
- Fix issues with responding with long strings [#576](https://github.com/kemalcr/kemal/pull/576). Thanks @mamantoha :pray:
- Fix broken WebSocket support in 0.35.0 [#577](https://github.com/kemalcr/kemal/pull/577). Thanks @mamantoha :pray:
- Allow to set optional response body on redirects [#561](https://github.com/kemalcr/kemal/pull/561). Thanks @mamantoha :pray:

# 0.26.1 (01-12-2019)

- Fix process request when a response already closed [#550](https://github.com/kemalcr/kemal/pull/550). Thanks @mamantoha :pray:
- Switch to new Ameba repository [#549](https://github.com/kemalcr/kemal/pull/549). Thanks @mamantoha :pray:
- Check for `KEMAL_ENV` variable already in `Config#initialize`[#552](https://github.com/kemalcr/kemal/pull/552). Thanks @Sija :pray:
- Cleanup Ameba warnings [#551](https://github.com/kemalcr/kemal/pull/551). Thanks @Sija :pray:
- Flush io buffer after each write to log [#554](https://github.com/kemalcr/kemal/pull/554). Thanks @mang :pray:

# 0.26.0 (05-08-2019)

- Crystal 0.30.0 support :tada: [#548](https://github.com/kemalcr/kemal/pull/548) and [#544](https://github.com/kemalcr/kemal/pull/544). Thanks @bcardiff and @straight-shoota :pray:
- Add support for serving files greater than 2^31 bytes [#546](https://github.com/kemalcr/kemal/pull/546). Thanks @omarroth :pray:
- Properly measure request time using `Time.monotonic` [#527](https://github.com/kemalcr/kemal/pull/527). Thanks @spinscale :pray:

# 0.25.2 (08-02-2019)

- Add option to config to parse or not command line parameters [#483](https://github.com/kemalcr/kemal/pull/483). Thanks @diegogub :pray:

- Allow to set filename for `send_file` [#512](https://github.com/kemalcr/kemal/pull/512). Thanks @mamantoha :pray:


```ruby
send_file env, "./asset/image.jpeg", filename: "image.jpg"
```

- Set `status_code` before response [#513](https://github.com/kemalcr/kemal/pull/513). Thanks @mamantohoa :pray:

- Use Crystal MIME registry. [#516](https://github.com/kemalcr/kemal/pull/516) Thanks @Sija :pray:

# 0.25.1 (06-10-2018)

- Fix `params.files` memoization https://github.com/kemalcr/kemal/pull/503. Thanks @mamantoha :pray:

# 0.25.0 (05-10-2018)

- Crystal 0.27.0 support.
-  *[breaking change]* Added back `env.params.files`.

Here's a fully working sample for reading a image file upload `image1` and saving it under `public/uploads`.

```crystal
post "/upload" do |env|
  file = env.params.files["image1"].tempfile
  file_path = ::File.join [Kemal.config.public_folder, "uploads/", File.basename(file.path)]
  File.open(file_path, "w") do |f|
    IO.copy(file, f)
  end
  "Upload ok"
end
```

To test

`curl -F "image1=@/Users/serdar/Downloads/kemal.png" http://localhost:3000/upload`

- Cache HTTP routes to increase performance :rocket: https://github.com/kemalcr/kemal/pull/493

# 0.24.0 (14-08-2018)

- *[breaking change]* Removed `env.params.files`. You can use Crystal's built-in `HTTP::FormData.parse` instead

```ruby
post "/upload" do |env|
  HTTP::FormData.parse(env.request) do |upload|
    filename = file.filename

    if !filename.is_a?(String)
      "No filename included in upload"
    else
      file_path = ::File.join [Kemal.config.public_folder, "uploads/", filename]
      File.open(file_path, "w") do |f|
      IO.copy(file.tmpfile, f)
    end
    "Upload OK"
  end
end
```

- *[breaking change]* From now on to access dynamic url params in a WebSocket route you have to use:

```ruby
ws "/:id" do |socket, context|
  id = context.ws_route_lookup.params["id"]
end
```

- *[breaking change]* Removed `_method` magic param.

- Added new exception page [#466](https://github.com/kemalcr/kemal/pull/466). Thanks @mamantoha 🙏

- Support custom port binding. Thanks @straight-shoota 🙏

```ruby
Kemal.run do |config|
  server = config.server.not_nil!
  server.bind_tcp "127.0.0.1", 3000, reuse_port: true
  server.bind_tcp "0.0.0.0", 3001, reuse_port: true
end
```

# 0.23.0 (17-06-2018)

- Crystal 0.25.0 support 🎉
- Add `Kemal::Context.get?` to safely access context storage :sunglasses:
- [Security] Don't serve 404 image dynamically :thumbsup:
- Disable `X-Powered-By` header [#449](https://github.com/kemalcr/kemal/pull/449). Thanks @Blacksmoke16 🙏

# 0.22.0 (29-12-2017)

- Crystal 0.24.1 support 🎉
- Only return string from route.[#408](https://github.com/kemalcr/kemal/pull/408) thanks @crisward 🙏
- Don't crash on empty path when compiled in --release. [#407](https://github.com/kemalcr/kemal/pull/407) thanks @crisward 🙏
- Rename `Kemal::CommonLogHandler` to `Kemal::LogHandler` and `Kemal::CommonExceptionHandler` to `Kemal::ExceptionHandler`.
- Allow videos to be opened with correct mime type. [#406](https://github.com/kemalcr/kemal/pull/406) thanks @crisward 🙏
- Add webm mime type.[#413](https://github.com/kemalcr/kemal/pull/413) thanks @reindeer-cafe 🙏


# 0.21.0 (05-09-2017)

- Dynamically insert handlers :muscle: Fixes [#376](https://github.com/kemalcr/kemal/pull/376).
- Add context to WebSocket. This allows one to use `HTTP::Server::Context` in `ws` declarations :heart_eyes: Fixes [#349](https://github.com/kemalcr/kemal/pull/349).

```ruby
ws "/:room_name" do |socket, env|
  env.params.url["room_name"]
end
```

- Add support for customizing the headers of built-in `Kemal::StaticFileHandler` :hammer: Useful for supporting `CORS` for single page applications :clap:

```ruby
static_headers do |response, filepath, filestat|
  if filepath =~ /\.html$/
      response.headers.add("Access-Control-Allow-Origin", "*")
    end
    response.headers.add("Content-Size", filestat.size.to_s)
  end
end
```

- Allow %w in Handler macros [#385](https://github.com/kemalcr/kemal/pull/385). Thanks @will :pray:

- Security: X-Content-Type-Options: nosniff for static files. Fixes [#379](https://github.com/kemalcr/kemal/issues/379). Thanks @crisward :pray:

- Performance: [Remove tempfile management to OS](https://github.com/kemalcr/kemal/commit/a1520de7ed3865fa73258343a80fad4f20666a99). This brings %10 - 15 performance boost to Kemal :rocket:

# 0.20.0 (01-07-2017)

- Crystal 0.23.0 support! As always, Kemal is compatible with the latest major release of Crystal 💎
- Great news everyone 🎉 All handlers are now completely ***customizable***!. Use the default `Kemal` handlers or go wild, it's all up to you ⛏

```ruby
# Don't forget to add `Kemal::RouteHandler::INSTANCE` or your routes won't work!
Kemal.config.handlers = [Kemal::InitHandler.new, YourHandler.new, Kemal::RouteHandler::INSTANCE]
```

You can also insert a handler into a specific position.

```ruby
# This adds MyCustomHandler instance to 1 position. Be aware that the index starts from 0.
add_handler MyCustomHandler.new, 1
```
- Updated [Kilt](https://github.com/jeromegn/kilt) to v0.4.0.
- Make `Route` a `Struct`. This improves the performance of route declarations.

# 0.19.0 (09-05-2017)

-  Return no body for head route fixes #323. (thanks @crisward)
-  Update `radix` to `0.3.8`. (thanks @waghanza)
-  User defined context store types. (thanks @neovitange)

```ruby
class User
   property name
end

add_context_storage_type(User)
```

- Prevent `send_file returning filesize. (thanks @crisward)
- Dont call setup in `config#add_filter_handler` fixes #338.

# 0.18.3 (07-03-2017)

- Remove `Gzip::Header` monkey patch since it's fixed in `Crystal 0.21.1`.

# 0.18.2 (24-02-2017)

- Fix [Gzip in Kemal Seems broken for static files](https://github.com/kemalcr/kemal/issues/316). This was caused by `Gzip::Writer` in `Crystal 0.21.0` and currently mitigated by monkey patching `Gzip::Header`.

# 0.18.1 (21-02-2017)

- Crystal 0.21.0 support
- Drop `multipart.cr` dependency. `multipart` support is now built-into Crystal <3
- Since Crystal 0.21.0 comes built-in with `multipart` there are some improvements and deprecations.

`meta` has been removed from `FileUpload` and it has the following properties

  + `tmpfile`: This is temporary file for file upload. Useful for saving the upload file.
  + `filename`: File name of the file upload. (logo.png, images.zip e.g)
  + `headers`: Headers for the file upload.
  + `creation_time`: Creation time of the file upload.
  + `modification_time`: Last Modification time of the file upload.
  + `read_time`: Read time of the file upload.
  + `size`: Size of the file upload.


# 0.18.0 (11-02-2017)

- Simpler file upload. File uploads can now be access from `HTTP::Server::Context` like `env.params.files["filename"]`.

`env.params.files["filename"]` has 5 methods

- `tmpfile`: This is temporary file for file upload. Useful for saving the upload file.
- `tmpfile_path`: File path of `tmpfile`.
- `filename`: File name of the file upload. (logo.png, images.zip e.g)
- `meta`: Meta information for the file upload.
- `headers`: Headers for the file upload.

Here's a fully working sample for reading a image file upload `image1` and saving it under `public/uploads`.

  ```crystal
post "/upload" do |env|
  file = env.params.files["image1"].tmpfile
  file_path = ::File.join [Kemal.config.public_folder, "uploads/", file.filename]
  File.open(file_path, "w") do |f|
    IO.copy(file, f)
  end
  "Upload ok"
end
  ```

To test

`curl -F "image1=@/Users/serdar/Downloads/kemal.png" http://localhost:3000/upload`

- RF7233 support a.k.a file streaming. (https://github.com/kemalcr/kemal/pull/299) (thanks @denysvitali)

- Update Radix to 0.3.7. Fixes https://github.com/kemalcr/kemal/issues/293
- Configurable startup / shutdown logging. https://github.com/kemalcr/kemal/issues/291 and https://github.com/kemalcr/kemal/issues/292 (thanks @twisterghost).

# 0.17.5 (09-01-2017)

- Update multipart.cr to 0.1.2. Fixes #285 related to multipart.cr

# 0.17.4 (24-12-2016)

- Support for Crystal 0.20.3
- Add `Kemal.stop`. Fixes #269.
- `HTTP::Handler` is not a class anymore, it's a module. See https://github.com/crystal-lang/crystal/releases/tag/0.20.3

# 0.17.3 (03-12-2016)

- Handle missing 404 image. Fixes #263
- Remove basic auth middleware from core and move to [kemalcr/kemal-basic-auth](https://github.com/kemalcr/kemal-basic-auth).

# 0.17.2 (25-11-2016)

- Use body.gets_to_end for parse_json. Fixes #260.
- Update Radix to 0.3.5 and lock pessimistically. (thanks @luislavena)

# 0.17.1 (24-11-2016)

- Treat `HTTP::Request` body as an `IO`. Fixes [#257](https://github.com/sdogruyol/kemal/issues/257)

# 0.17.0 (23-11-2016)

- Reimplemented Request middleware / filter routing.

Now all requests will first go through the Middleware stack then Filters (before_*) and will finally reach the matching route.

Which is illustrated as,

```
Request -> Middleware -> Filter -> Route
```

- Rename `return_with` as `halt`.
- Route declaration must start with `/`.  Fixes [#242](https://github.com/sdogruyol/kemal/issues/242)
- Set default exception Content-Type to text/html. Fixes [#202](https://github.com/sdogruyol/kemal/issues/242)
- Add `only` and `exclude` paths for `Kemal::Handler`. This change requires that all handlers must inherit from `Kemal::Handler`.

For example this handler will only work on `/` path. By default the HTTP method is `GET`.


```crystal
class OnlyHandler < Kemal::Handler
  only ["/"]

  def call(env)
    return call_next(env) unless only_match?(env)
    puts "If the path is / i will be doing some processing here."
  end
end
```

The handlers using `exclude` will work on the paths that isn't specified. For example this handler will work on any routes other than `/`.

```crystal
class ExcludeHandler < Kemal::Handler
  exclude ["/"]

  def call(env)
    return call_next(env) unless only_match?(env)
    puts "If the path is NOT / i will be doing some processing here."
  end
end
```

- Close response on `halt`. (thanks @samueleaton).
- Update `Radix` to `v0.3.4`.
- `error` handler now also yields error. For example you can get the error mesasage like

```crystal
  error 500 do |env, err|
    err.message
  end
```

- Update `multipart.cr` to `v0.1.1`

# 0.16.1 (12-10-2016)

- Improved Multipart support with more info on parsed files. `parse_multipart(env)` now yields
an `UploadFile` object which has the following properties `field`,`data`,`meta`,`headers.

```crystal
post "/upload" do |env|
  parse_multipart(env) do |f|
    image1 = f.data if f.field == "image1"
    image2 = f.data if f.field == "image2"
    puts f.meta
    puts f.headers
    "Upload complete"
  end
end
```

# 0.16.0

- Multipart support <3 (thanks @RX14). Now you can handle file uploads.

```crystal
post "/upload" do |env|
  parse_multipart(env) do |field, data|
    image1 = data if field == "image1"
    image2 = data if field == "image2"
    "Upload complete"
  end
end
```

- Make session configurable. Now you can specify session name and expire time wit

```crystal
Kemal.config.session["name"] = "your_app"
Kemal.config.session["expire_time"] = 48.hours
```

- Session now supports more types. (String, Int32, Float64, Bool)
- Add `gzip` helper to enable / disable gzip compression on responses.
- Static file caching with etag and gzip (thanks @crisward)
- `Kemal.run` now accepts port to listen.

# 0.15.1 (05-09-2016)

- Don't forget to call_next on NullLogHandler

# 0.15.0 (03-09-2016)

- Add context store
- `KEMAL_ENV` respects to `Kemal.config.env` and needs to be explicitly set.
- `Kemal::InitHandler` is introduced. Adds initial configuration, headers like `X-Powered-By`.
- Add `send_file` to helpers.
- Add mime types.
- Fix parsing JSON params when "charset" is present in "Content-Type" header.
- Use http-only cookie for session
- Inject STDOUT by default in CommonLogHandler
