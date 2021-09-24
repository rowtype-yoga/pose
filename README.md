# POSE - a formatter for PureScript code

_Disclaimer_: You might want to use the [tidy](https://github.com/natefaubion/purescript-tidy) formatter instead.

**The POSE project is in low maintenance mode.** This means that pull requests are very welcome but the original authors don't actively improve the code.

## Try it out

Try the formatter live and find some instructions [here](https://pose.rowtype.yoga)

## Installation

We hook into prettier to take care of a lot of the heavy lifting for us so let's install it and the plugin:

```
npm install --save-dev prettier @rowtype-yoga/prettier-plugin-purescript
```

We only want to format our own code so we modify .prettierignore:

```
echo -e ".spago\noutput/" >> .prettierignore
```

## Formatting

If you use VSCode then you can simply turn on format on save and you're good to go, if you want to manually format some files here's an example:

```
npx prettier --write src
```

## Contributing

The project is split in two subpackages:

1. The core formatter in the `packages/core` folder
2. The prettier plugin in the `packages/prettier-plugin-purescript` folder

To work on the formatter add a test in `packages/core/testfiles/original/` and run:

```sh
spago -x exe.dhall test
```

Then you should adjust the corresponding file in: `packages/core/testfiles/golden/` to what you expect the output to be and start making changes in `packages/core/src/Format.purs` until all tests pass.

Run the tests often to get nice diffs:
![image of a diff](../../blob/main/images/diff.png)

```
51 passing
1 failed

1) Golden Tests formats Issue5.purs
  "The diff is saved here: file:///tmp/testfiles-golden-Issue5.purs-actual.html"
```
