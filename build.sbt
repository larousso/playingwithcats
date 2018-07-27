
name := "test-cats"
scalaVersion := "2.12.6"

val akkaVersion = "2.5.12"

libraryDependencies ++= Seq(
    "org.typelevel"            %% "cats-core"                     % "1.1.0",
    "org.typelevel"            %% "cats-effect"                   % "1.0.0-RC2",
    "org.typelevel"            %% "cats-effect-laws"              % "1.0.0-RC2",
    "org.typelevel"            %% "cats-mtl-core"                 % "0.2.1",
    "co.fs2"                   %% "fs2-core"                      % "0.10.4",
    "com.typesafe.akka"        %% "akka-actor"                    % akkaVersion,
    "com.typesafe.akka"        %% "akka-stream"                   % akkaVersion,
    "org.scala-lang.modules" %% "scala-xml" % "1.1.0"
)

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions ++= Seq(
    "-feature",
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    //"-deprecation",
    "-encoding",
    "utf8",
    "-Ypartial-unification",
    //"-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver.
    //"-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-unused:imports",
    //"-Ywarn-nullary-override", // Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions" // Allow definition of implicit functions called views
)


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
