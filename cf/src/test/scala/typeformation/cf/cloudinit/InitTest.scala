package typeformation.cf.cloudinit

import io.circe.Json
import org.scalatest.{FreeSpec, Matchers}
import io.circe.syntax._
import typeformation.cf.syntax._
import io.circe.parser.parse
import typeformation.cf.{JsonTest, Parameter}
import typeformation.cf.Encoding._
import typeformation.cf.cloudinit.Init.{Config, ConfigSet}

class InitTest extends FreeSpec with JsonTest with Matchers {
  "Command is supported" in {
    val expected =
      """
        | {
        |   "cmd" : {
        |    "command": "echo \"$MAGIC\" > test.txt",
        |    "env" : { "MAGIC" : "I come from the environment!" },
        |    "cwd" : "~",
        |    "test" : "test ! -e ~/test.txt"
        |   }
        | }
        |
      """.stripMargin
    Right(encode(
      Command(
        logicalId = "cmd",
        command = """echo "$MAGIC" > test.txt""",
        cwd = Some("~"),
        env = Map("MAGIC" -> lit("I come from the environment!")),
        test = Some("test ! -e ~/test.txt")))) should === (parse(expected))
  }
  "File is supported" in {
    val dbNameParam = Parameter.Str("DBName")

    val expected = """
      |{
      |  "/tmp/setup.mysql" : {
      |    "content" : { "Fn::Join" : ["", ["CREATE DATABASE ", { "Ref" : "DBName" }, ";\n" ]]},
      |    "mode"  : "000644",
      |    "owner" : "root",
      |    "group" : "root"
      |  }
      |}
    """.stripMargin

    Right(encode(File.FromString(
      path = "/tmp/setup.mysql",
      content = fnJoin("", (lit("CREATE DATABASE "), dbNameParam.ref, lit(";\n"))),
      mode = Some("000644"),
      owner = Some("root"),
      group = Some("root")
    ))) should === (parse(expected))
  }

  "Package is supported" in {
    val expected1 =
      """
        |{
        |  "epel" : "http://download.fedoraproject.org/pub/epel/5/i386/epel-release-5-4.noarch.rpm"
        |}
      """.stripMargin

    Right(Package(
      name = "epel",
      format = rpm,
      version = List("http://download.fedoraproject.org/pub/epel/5/i386/epel-release-5-4.noarch.rpm")
    ).asJson) should ===(parse(expected1))

     Right(Package(
      name = "chef",
      format = rubygems,
      version = List("0.10.2")
    ).asJson) should ===(parse(""" { "chef" : ["0.10.2"] }"""))
  }

  "Service is supported" in {
    val expected =
      """
        |{
        |  "sysvinit" : {
        |    "nginx" : {
        |      "enabled" : true,
        |      "ensureRunning" : true,
        |      "files" : ["/etc/nginx/nginx.conf"],
        |      "sources" : ["/var/www/html"]
        |    },
        |    "php-fastcgi" : {
        |      "enabled" : true,
        |      "ensureRunning" : true,
        |      "packages" : { "yum" : ["php", "spawn-fcgi"] }
        |    },
        |    "sendmail" : {
        |      "enabled" : false,
        |      "ensureRunning" : false
        |    }
        |  }
        |}
        |
      """.stripMargin

    Right(encode(Service.Sysvinit(List(
      Service(
        name = "nginx",
        enabled = true,
        ensureRunning = true,
        files = List("/etc/nginx/nginx.conf"),
        sources = List("/var/www/html")
      ),
       Service(
         name = "php-fastcgi",
         enabled = true,
         ensureRunning = true,
         packages = Map(yum -> List("php", "spawn-fcgi"))

       ),
       Service(
        name = "sendmail",
        enabled = false,
        ensureRunning = false
       )
    )))) should === (parse(expected))
  }

  "Init.Config is supported" in {
    val expected =
      """
        | {
        |   "Install" : {
        |     "packages" : {
        |       "yum" : {
        |         "mysql"        : [],
        |         "mysql-server" : [],
        |         "mysql-libs"   : [],
        |         "httpd"        : [],
        |         "php"          : [],
        |         "php-mysql"    : []
        |       }
        |     },
        |     "files" : {
        |       "/var/www/html/index.php" : {
        |         "source": "http://example.com/index.php",
        |         "mode"  : "000600",
        |         "owner" : "apache",
        |         "group" : "apache"
        |       }
        |     },
        |     "services" : {
        |       "sysvinit" : {
        |         "httpd"   : { "enabled" : true, "ensureRunning" : true }
        |       }
        |     }
        |   }
        | }
      """.stripMargin

    val packages = List("mysql", "mysql-server", "mysql-libs", "httpd", "php", "php-mysql").map {
      Package(_, yum)
    }

    Right(encode(Init.Config(
      logicalId = "Install",
      packages = packages,
      files = List(
        File.FromUrl(
          path = "/var/www/html/index.php",
          source = "http://example.com/index.php",
          mode = Some("000600"),
          owner = Some("apache"),
          group = Some("apache")
        )
      ),
      services = Service.Sysvinit(List(
        Service("httpd", enabled = true, ensureRunning = true)
      ))
    ))) should ===(parse(expected))
  }

  "Init is handled" in {
    val expected =
      """
        | {
        |   "AWS::CloudFormation::Init" : {
        |    "configSets" : {
        |        "ascending" : [ "config1" , "config2" ],
        |        "descending" : [ "config2" , "config1" ]
        |    },
        |    "config1" : {
        |        "commands" : {
        |            "test" : {
        |                "command" : "echo 1"
        |            }
        |        },
        |        "packages": {},
        |        "services": {}
        |    },
        |    "config2" : {
        |        "commands" : {
        |            "test" : {
        |                "command" : "echo 2"
        |            }
        |        },
        |        "packages": {},
        |        "services": {}
        |    }
        |  }
        |}
      """.stripMargin

    val config1 = Config(
      logicalId = "config1",
      commands = List(
        Command(logicalId = "test", command = "echo 1")
      )
    )

    val config2 = Config(
      logicalId = "config2",
      commands = List(
        Command(logicalId = "test", command = "echo 2")
      )
    )

    Right(encode(Init(List(
      ConfigSet("ascending", List(config1, config2)),
      ConfigSet("descending", List(config2, config1))
    )))) should ===(parse(expected))
  }
}
