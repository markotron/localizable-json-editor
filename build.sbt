name := "BBTMS"

version := "1.0"

scalaVersion := "2.12.1"

enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "com.thoughtworks.binding" %%% "dom" % "latest.release",
  "com.github.japgolly.scalajs-react" %%% "core" % "1.0.0-RC1"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

jsDependencies ++= Seq(

  "org.webjars.bower" % "react" % "15.4.2"
    /        "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",

  "org.webjars.bower" % "react" % "15.4.2"
    /         "react-dom.js"
    minified  "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM",

  "org.webjars.bower" % "react" % "15.4.2"
    /         "react-dom-server.js"
    minified  "react-dom-server.min.js"
    dependsOn "react-dom.js"
    commonJSName "ReactDOMServer"
)
