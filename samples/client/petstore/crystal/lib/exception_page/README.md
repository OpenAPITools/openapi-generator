# Exception Page

A library for displaying exceptional exception pages for easier debugging.

![screen shot 2018-06-29 at 2 39 18 pm](https://user-images.githubusercontent.com/22394/42109073-6e767d06-7baa-11e8-9ec9-0a2afce605be.png)

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  exception_page:
    github: crystal-loot/exception_page
```

## Usage

Require the shard:

```crystal
require "exception_page"
```

Create an exception page:

```crystal
class MyApp::ExceptionPage < ExceptionPage
  def styles : Styles
    ExceptionPage::Styles.new(
      accent: "purple", # Choose the HTML color value. Can be hex
    )
  end
end
```

Render the HTML when an exception occurs:

```crystal
class MyErrorHandler
  include HTTP::Handler

  def call_next(context)
    begin
      # Normally you'd call some code to handle the request
      # We're hard-coding an error here to show you how to use the lib.
      raise SomeError.new("Something went wrong")
    rescue e
      context.response.status_code = 500
      context.response.print MyApp::ExceptionPage.for_runtime_exception(context, e).to_s
    end
  end
```

## Customizing the page

```crystal
class MyApp::ExceptionPage < ExceptionPage
  def styles : Styles
    ExceptionPage::Styles.new(
      accent: "purple", # Required
      highlight: "gray", # Optional
      flash_highlight: "red", # Optional
      logo_uri: "base64_encoded_data_uri" # Optional. Defaults to Crystal logo. Generate a logo here: https://dopiaza.org/tools/datauri/index.php
    )
  end

  # Optional. If provided, clicking the logo will open this page
  def project_url
    "https://myproject.com"
  end

  # Optional
  def stack_trace_heading_html
    <<-HTML
    <a href="#" onclick="sayHi()">Say hi</a>
    HTML
  end

  # Optional
  def extra_javascript
    <<-JAVASCRIPT
    window.sayHi = function() {
      alert("Say Hi!");
    }
    JAVASCRIPT
  end
end
```

## Development

TODO: Write development instructions here

## Contributing

1.  Fork it (<https://github.com/crystal-loot/exception_page/fork>)
2.  Create your feature branch (`git checkout -b my-new-feature`)
3.  Commit your changes (`git commit -am 'Add some feature'`)
4.  Push to the branch (`git push origin my-new-feature`)
5.  Create a new Pull Request

## Contributors

- [@paulcsmith](https://github.com/paulcsmith) Paul Smith
- [@faustinoaq](https://github.com/faustinoaq) Faustino Aigular - Wrote the initial [Amber PR adding exception pages](https://github.com/amberframework/amber/pull/864)

## Special Thanks

This exception page is heavily based on the [Phoenix error page](https://github.com/phoenixframework/phoenix/issues/1776)
by [@rstacruz](https://github.com/rstacruz). Thanks to the Phoenix team and @rstacruz!
