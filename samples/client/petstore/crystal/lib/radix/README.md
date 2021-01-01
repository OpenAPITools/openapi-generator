# Radix Tree

[Radix tree](https://en.wikipedia.org/wiki/Radix_tree) implementation for
Crystal language

[![Build Status](https://img.shields.io/travis/luislavena/radix/master.svg)](https://travis-ci.org/luislavena/radix)
[![Latest Release](https://img.shields.io/github/release/luislavena/radix.svg)](https://github.com/luislavena/radix/releases)

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  radix:
    github: luislavena/radix
```

## Usage

### Building Trees

You can associate a *payload* with each path added to the tree:

```crystal
require "radix"

tree = Radix::Tree(Symbol).new
tree.add "/products", :products
tree.add "/products/featured", :featured

result = tree.find "/products/featured"

if result.found?
  puts result.payload # => :featured
end
```

The types allowed for payload are defined on Tree definition:

```crystal
tree = Radix::Tree(Symbol).new

# Good, since Symbol is allowed as payload
tree.add "/", :root

# Compilation error, Int32 is not allowed
tree.add "/meaning-of-life", 42
```

Can combine multiple types if needed:

```crystal
tree = Radix::Tree(Int32 | String | Symbol).new

tree.add "/", :root
tree.add "/meaning-of-life", 42
tree.add "/hello", "world"
```

### Lookup and placeholders

You can also extract values from placeholders (as named segments or globbing):

```crystal
tree.add "/products/:id", :product

result = tree.find "/products/1234"

if result.found?
  puts result.params["id"]? # => "1234"
end
```

Please see `Radix::Tree#add` documentation for more usage examples.

## Caveats

Pretty much all Radix implementations have their limitations and this project
is no exception.

When designing and adding *paths* to a Tree, please consider that two different
named parameters cannot share the same level:

```crystal
tree.add "/", :root
tree.add "/:post", :post
tree.add "/:category/:post", :category_post # => Radix::Tree::SharedKeyError
```

This is because different named parameters at the same level will result in
incorrect `params` when lookup is performed, and sometimes the value for
`post` or `category` parameters will not be stored as expected.

To avoid this issue, usage of explicit keys that differentiate each path is
recommended.

For example, following a good SEO practice will be consider `/:post` as
absolute permalink for the post and have a list of categories which links to
the permalinks of the posts under that category:

```crystal
tree.add "/", :root
tree.add "/:post", :post                    # this is post permalink
tree.add "/categories", :categories         # list of categories
tree.add "/categories/:category", :category # listing of posts under each category
```

## Implementation

This project has been inspired and adapted from
[julienschmidt/httprouter](https://github.com/julienschmidt/httprouter) and
[spriet2000/vertx-http-router](https://github.com/spriet2000/vertx-http-router)
Go and Java implementations, respectively.

Changes to logic and optimizations have been made to take advantage of
Crystal's features.

## Contributing

1. Fork it ( https://github.com/luislavena/radix/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Contributors

- [Luis Lavena](https://github.com/luislavena) - creator, maintainer
