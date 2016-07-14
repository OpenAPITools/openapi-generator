# Container Interoperability

[![Latest Stable Version](https://poser.pugx.org/container-interop/container-interop/v/stable.png)](https://packagist.org/packages/container-interop/container-interop)

*container-interop* tries to identify and standardize features in *container* objects (service locators,
dependency injection containers, etc.) to achieve interopererability.

Through discussions and trials, we try to create a standard, made of common interfaces but also recommendations. 

If PHP projects that provide container implementations begin to adopt these common standards, then PHP
applications and projects that use containers can depend on the common interfaces instead of specific
implementations. This facilitates a high-level of interoperability and flexibility that allows users to consume
*any* container implementation that can be adapted to these interfaces.

The work done in this project is not officially endorsed by the [PHP-FIG](http://www.php-fig.org/), but it is being
worked on by members of PHP-FIG and other good developers. We adhere to the spirit and ideals of PHP-FIG, and hope
this project will pave the way for one or more future PSRs.


## Installation

You can install this package through Composer:

```json
{
    "require": {
        "container-interop/container-interop": "~1.0"
    }
}
```

The packages adheres to the [SemVer](http://semver.org/) specification, and there will be full backward compatibility
between minor versions.

## Standards

### Available

- [`ContainerInterface`](src/Interop/Container/ContainerInterface.php).
[Description](docs/ContainerInterface.md) [Meta Document](docs/ContainerInterface-meta.md).
Describes the interface of a container that exposes methods to read its entries.
- [*Delegate lookup feature*](docs/Delegate-lookup.md).
[Meta Document](docs/Delegate-lookup-meta.md).
Describes the ability for a container to delegate the lookup of its dependencies to a third-party container. This 
feature lets several containers work together in a single application.

### Proposed

View open [request for comments](https://github.com/container-interop/container-interop/labels/RFC)

## Compatible projects

### Projects implementing `ContainerInterface`

- [Acclimate](https://github.com/jeremeamia/acclimate-container)
- [dcp-di](https://github.com/estelsmith/dcp-di)
- [Mouf](http://mouf-php.com)
- [Njasm Container](https://github.com/njasm/container)
- [PHP-DI](http://php-di.org)
- [PimpleInterop](https://github.com/moufmouf/pimple-interop)
- [XStatic](https://github.com/jeremeamia/xstatic)

### Projects implementing the *delegate lookup* feature

- [Mouf](http://mouf-php.com)
- [PHP-DI](http://php-di.org)
- [PimpleInterop](https://github.com/moufmouf/pimple-interop)

## Workflow

Everyone is welcome to join and contribute.

The general workflow looks like this:

1. Someone opens a discussion (GitHub issue) to suggest an interface
1. Feedback is gathered
1. The interface is added to a development branch
1. We release alpha versions so that the interface can be experimented with
1. Discussions and edits ensue until the interface is deemed stable by a general consensus
1. A new minor version of the package is released

We try to not break BC by creating new interfaces instead of editing existing ones.

While we currently work on interfaces, we are open to anything that might help towards interoperability, may that
be code, best practices, etc.
