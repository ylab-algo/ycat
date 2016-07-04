import sbt._

object YBuild extends Build {
  lazy val root =
    Project(id = "ycat", base = file("."))
      .dependsOn(ycatcont)

  lazy val ycatcont = Project(id = "ycat-container",
    base = file("ycat-container"))
    .dependsOn(ytrie, ykmer, yalignment, ycommon)

  lazy val ytrie = Project(id = "ytrie",
    base = file("ytrie"))
      .dependsOn(ycommon)

  lazy val ykmer = Project(id = "ykmer",
    base = file("ykmer"))
      .dependsOn(ycommon)

  lazy val yalignment = Project(id = "yalignment",
    base = file("yalignment"))
      .dependsOn(ycommon)

  lazy val ycommon = Project(id = "ycommon",
    base = file("ycommon"))
}