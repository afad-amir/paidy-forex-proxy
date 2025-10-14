addSbtPlugin("com.lucidchart"   % "sbt-scalafmt-coursier" % "1.16")
addSbtPlugin("com.timushev.sbt" % "sbt-updates"           % "0.5.3")
addSbtPlugin("io.spray"         % "sbt-revolver"          % "0.9.1")

// Code quality and dependency analysis
addSbtPlugin("ch.epfl.scala"     % "sbt-scalafix"              % "0.11.1")
addSbtPlugin("net.virtual-void"  % "sbt-dependency-graph"      % "0.10.0-RC1")
addSbtPlugin("com.github.cb372"  % "sbt-explicit-dependencies" % "0.2.16")

// Test coverage
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.3")