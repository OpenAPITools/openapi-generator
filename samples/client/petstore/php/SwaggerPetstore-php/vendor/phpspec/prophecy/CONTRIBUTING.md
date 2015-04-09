Contributing
------------

Prophecy is an open source, community-driven project. If you'd like to contribute,
feel free to do this, but remember to follow this few simple rules:

- Make your feature addition or bug fix,
- Add either specs or examples for any changes you're making (bugfixes or additions)
  (please look into `spec/` folder for some examples). This is important so we don't break
  it in a future version unintentionally,
- Commit your code, but do not mess with `CHANGES.md`,

Running tests
-------------

Make sure that you don't break anything with your changes by running:

```bash
$> composer install --dev --prefer-dist
$> vendor/bin/phpspec
```
