//> using scala 3.3.3
//> using dep "org.jsoup:jsoup:1.17.2"

import org.jsoup.Jsoup
import scala.jdk.CollectionConverters._
import scala.sys.process._
import java.nio.file.Files
import java.nio.file.Paths
import scala.language.postfixOps

object UpdateMetals {
  def getMetalsPath(): String = {
    val home = System.getProperty("user.home")
    s"${home}/.local/bin/metals-emacs"
  }

  // based on xml
  def getLatestVersion1(): Option[String] = {
    val url =
      "https://oss.sonatype.org/content/repositories/snapshots/org/scalameta/metals_2.13/maven-metadata.xml"
    val doc = Jsoup.connect(url).get()
    val latest = doc.select("versioning latest")
    if (latest.isEmpty()) None
    else Some(latest.first().text())
  }

  // based on html
  def getLatestVersion(): Option[String] = {
    val url =
      "https://scalameta.org/metals/docs/"
    val doc = Jsoup.connect(url).get()
    doc
      .select("tbody > tr:nth-child(2) > td:nth-child(1)")
      .asScala
      .map(_.text())
      .headOption
  }

  def getCurrentVersion(): Option[String] = {
    val metals = getMetalsPath()
    // val metals = "/usr/local/bin/metals-emacs"
    if (!Files.exists(Paths.get(metals))) None
    else {
      val versionInfo = Process(s"${metals} -v").lineStream.head
      versionInfo match {
        case s"metals $version" => Some(version)
        case _                  => None
      }
    }
  }

  def updateMetals(version: String): Unit = {
    if (!Files.exists(Paths.get("./coursier"))) {
      "curl -L -o coursier https://git.io/coursier-cli" #&& "chmod +x coursier" !
    }

    s"""./coursier bootstrap
      --java-opt -XX:+UseZGC
      --java-opt -XX:+UseStringDeduplication
      --java-opt -Xss8m
      --java-opt -Xms256m
      --java-opt -Dmetals.client=emacs
      org.scalameta:metals_2.13:${version}
      -r bintray:scalacenter/releases
      -r sonatype:snapshots
      -o ./metals-emacs -f""" #&&
      "chmod +x metals-emacs" #&&
      s"mv metals-emacs ${getMetalsPath()}" !
  }

  def main(args: Array[String]): Unit = {
    if (args.size == 1) {
       updateMetals(args(0))
       return
    } 

    val current = getCurrentVersion().getOrElse("")
    getLatestVersion() match {
      case Some(latest) => {
        println(s"Current: ${current}")
        println(s"Latest:  ${latest}")
        if (current != latest) {
          updateMetals(latest)
        } else {
          println("Nothing to upgrade")
        }
      }
      case _ => println("Can't find latest version")
    }
  }
}
