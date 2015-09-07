# SwaggerClient

## Requirements

The API client library requires ARC (Automatic Reference Counting) to be enabled in your Xcode project.

## Installation

To install it, put the API client library in your project and then simply add the following line to your Podfile:

```ruby
pod "SwaggerClient", :path => "/path/to/lib"
```

## Recommendation

It's recommended to create an instance of ApiClient per thread in a multithreaded environment to avoid any potential issue.

## Author

apiteam@swagger.io


