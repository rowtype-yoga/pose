const withBundleAnalyzer = require("@next/bundle-analyzer")({
  enabled: process.env.ANALYZE === "true",
})
const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin")
const withTM = require("next-transpile-modules")([
  // `monaco-editor` isn't published to npm correctly: it includes both CSS
  // imports and non-Node friendly syntax, so it needs to be compiled.
  "monaco-editor",
])

module.exports = withTM(
  withBundleAnalyzer({
    target: "serverless",
    webpack(config, { isServer }) {
      // https://github.com/purescript-contrib/purescript-affjax/issues/63
      if (isServer) {
        config.module.rules.push({
          test: /output\/Affjax\/foreign\.js$/,
          loader: "string-replace-loader",
          options: {
            search: "module.require",
            replace: "__webpack_require__",
          },
        })
      }
      const rule = config.module.rules
        .find((rule) => rule.oneOf)
        .oneOf.find(
          (r) =>
            // Find the global CSS loader
            r.issuer && r.issuer.include && r.issuer.include.includes("_app")
        )
      if (rule) {
        rule.issuer.include = [
          rule.issuer.include,
          // Allow `monaco-editor` to import global CSS:
          /[\\/]node_modules[\\/]monaco-editor[\\/]/,
        ]
      }

      config.plugins.push(
        new MonacoWebpackPlugin({
          languages: ["purescript"],
          filename: "static/[name].worker.js",
        })
      )

      return config
    },
  })
)
