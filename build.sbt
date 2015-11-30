name := "chinese-restaurant-process"

organization := "com.monsanto.stats"

scalaVersion := "2.11.7"

libraryDependencies ++= {
  object v {
    val scalatest = "3.0.0-M5"
    val scalanlp = "0.11.2"
    val scalacheck = "1.12.3"
  }
  Seq(
    "org.scalactic"     %% "scalactic"                          % v.scalatest,
    "org.scalanlp"      %% "breeze"                             % v.scalanlp,
    // native libraries are not included by default. add this if you want them (as of 0.7)
    // native libraries greatly improve performance, but increase jar sizes.
    // It also packages various blas implementations, which have licenses that may or may not
    // be compatible with the Apache License. No GPL code, as best I know.
    "org.scalanlp"      %% "breeze-natives"                     % v.scalanlp,
    // the visualization library is distributed separately as well.
    // It depends on LGPL code.
    "org.scalanlp"      %% "breeze-viz"                         % v.scalanlp,
    "org.scalatest"     %% "scalatest"                          % v.scalatest % "test",
    "org.scalacheck"    %% "scalacheck"                         % v.scalacheck % "test",
    "org.spire-math"    %% "debox"                              % "0.7.3"
  )
}

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
   {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case PathList("reference.conf") => MergeStrategy.concat
    case x => MergeStrategy.first
   }
}

fork in run := true

javaOptions += "-Xmx4G"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oKT")

parallelExecution in Test := false

// to elide nothing (include everything)
scalacOptions ++= Seq("-Xelide-below", s"${scala.annotation.elidable.MINIMUM}")

// to turn off requirements checks in CRP for extra performance (elide everything that's elidable)
// This didn't really pay substantially as yet, but it may later once we're faster. Will
// leave it in here commented out for future reference until such time as we're sure we
// don't ever want this, at which point we should remove it.
// scalacOptions ++= Seq("-Xelide-below", s"${scala.annotation.elidable.MAXIMUM}")

test in assembly := {}

// for ghpages

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:MonsantoCo/chinese-restaurant-process.git"

// for bintray

bintrayOrganization := Some("monsanto")

licenses += ("BSD", url("http://opensource.org/licenses/BSD-3-Clause"))
