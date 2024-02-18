// Main
//> using scala "3.3.2"
//> using options -source:future -Yexplicit-nulls
//> using options -project enhanced-string-interpolator -siteroot ${.}

//> using publish.ci.computeVersion "git:tag"
//> using publish.ci.password "env:PUBLISH_PASSWORD"
//> using publish.ci.publicKey "env:PUBLISH_PUBLIC_KEY"
//> using publish.ci.repository "central-s01"
//> using publish.ci.secretKey "env:PUBLISH_SECRET_KEY"
//> using publish.ci.secretKeyPassword "env:PUBLISH_SECRET_KEY_PASSWORD"
//> using publish.ci.user "env:PUBLISH_USER"
//> using publish.developers "bishabosha|Jamie Thompson|https://github.com/bishabosha"
//> using publish.license "Apache-2.0"
//> using publish.name "enhanced-string-interpolator"
//> using publish.organization "io.github.bishabosha"
//> using publish.url "https://github.com/bishabosha/enhanced-string-interpolator"
//> using publish.vcs "github:bishabosha/enhanced-string-interpolator"

// Test
//> using test.dependency "org.scalameta::munit:1.0.0-M10"
