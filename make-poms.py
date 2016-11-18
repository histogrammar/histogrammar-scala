#!/usr/bin/env python

VERSION = "1.0.4"

profiles = '''  <profiles>
    <profile>
      <id>scala-2.10</id>
      <activation>
        <activeByDefault>true</activeByDefault>
      </activation>
      <properties>
        <scala.version>2.10.6</scala.version>
        <scala.binary.version>2.10</scala.binary.version>
        <maven.compiler.source>1.7</maven.compiler.source>
        <maven.compiler.target>1.7</maven.compiler.target>
        <spark.version>1.6.2</spark.version>
      </properties>
    </profile>

    <profile>
      <id>scala-2.11</id>
      <activation>
        <property><name>scala-2.11</name></property>
      </activation>
      <properties>
        <scala.version>2.11.8</scala.version>
        <scala.binary.version>2.11</scala.binary.version>
        <maven.compiler.source>1.8</maven.compiler.source>
        <maven.compiler.target>1.8</maven.compiler.target>
        <spark.version>2.0.0</spark.version>
      </properties>
    </profile>

  </profiles>

'''

javaversion17 = '''    <maven.compiler.source>1.7</maven.compiler.source>
    <maven.compiler.target>1.7</maven.compiler.target>
'''

javaversion18 = '''    <maven.compiler.source>1.8</maven.compiler.source>
    <maven.compiler.target>1.8</maven.compiler.target>
'''

sourcejar = '''          <execution>
            <id>attach-sources</id>
            <goals>
              <goal>add-source</goal>
            </goals>
          </execution>
'''

javadocjar = '''          <execution>
            <id>attach-javadocs</id>
            <goals>
              <goal>doc-jar</goal>
            </goals>
          </execution>
'''

scalatest = '''      <plugin>
        <groupId>org.scalatest</groupId>
        <artifactId>scalatest-maven-plugin</artifactId>
        <version>1.0</version>
        <configuration>
          <reportsDirectory>${project.build.directory}/surefire-reports</reportsDirectory>
          <junitxml>.</junitxml>
        </configuration>
        <executions>
          <execution>
            <id>test</id>
            <goals>
              <goal>test</goal>
            </goals>
          </execution>
        </executions>
      </plugin>
      
'''

copydependencies = '''      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-dependency-plugin</artifactId>
        <version>2.10</version>
        <executions>
          <execution>
            <phase>package</phase>
            <goals>
              <goal>copy-dependencies</goal>
            </goals>
            <configuration>
              <outputDirectory>
                target/lib
              </outputDirectory>
            </configuration>
          </execution>
        </executions>
      </plugin>
'''

gpgplugin = '''      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-gpg-plugin</artifactId>
        <version>1.6</version>
        <executions>
          <execution>
            <id>sign-artifacts</id>
            <phase>verify</phase>
            <goals>
              <goal>sign</goal>
            </goals>
          </execution>
        </executions>
      </plugin>

'''

stagingplugin = '''      <plugin>
        <groupId>org.sonatype.plugins</groupId>
        <artifactId>nexus-staging-maven-plugin</artifactId>
        <version>1.6.7</version>
        <extensions>true</extensions>
        <configuration>
          <serverId>ossrh</serverId>
          <nexusUrl>https://oss.sonatype.org/</nexusUrl>
          <autoReleaseAfterClose>true</autoReleaseAfterClose>
        </configuration>
      </plugin>

'''

pluginmanagement = '''    <pluginManagement>  
      <plugins>
        <plugin>
          <groupId>org.apache.maven.plugins</groupId>
          <artifactId>maven-release-plugin</artifactId>
          <version>2.5</version>
          <configuration>
            <useReleaseProfile>false</useReleaseProfile>
            <pushChanges>false</pushChanges>
            <localCheckout>true</localCheckout>
            <goals>deploy</goals>
          </configuration>
        </plugin>
      </plugins>
    </pluginManagement> 

'''

distributionmanagement = '''  <distributionManagement>
    <snapshotRepository>
      <id>ossrh</id>
      <url>https://oss.sonatype.org/content/repositories/snapshots</url>
    </snapshotRepository>
    <repository>
      <id>ossrh</id>
      <url>https://oss.sonatype.org/service/local/staging/deploy/maven2/</url>
    </repository>
  </distributionManagement>

'''

template = '''<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>

  <!-- Copyright 2016 Jim Pivarski                                                 -->
  <!--                                                                             -->
  <!-- Licensed under the Apache License, Version 2.0 (the "License");             -->
  <!-- you may not use this file except in compliance with the License.            -->
  <!-- You may obtain a copy of the License at                                     -->
  <!--                                                                             -->
  <!--     http://www.apache.org/licenses/LICENSE-2.0                              -->
  <!--                                                                             -->
  <!-- Unless required by applicable law or agreed to in writing, software         -->
  <!-- distributed under the License is distributed on an "AS IS" BASIS,           -->
  <!-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    -->
  <!-- See the License for the specific language governing permissions and         -->
  <!-- limitations under the License.                                              -->

  <name>{name}</name>
  <description>{description}</description>
  <url>http://histogrammar.org</url>
  <inceptionYear>2016</inceptionYear>

  <groupId>org.diana-hep</groupId>
  <artifactId>{artifactid}</artifactId>
  <version>{version}</version>
  <packaging>jar</packaging>

  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>

  <developers>
    <developer>
      <name>Jim Pivarski</name>
      <email>jpivarski@gmail.com</email>
      <organization>DIANA-HEP</organization>
      <organizationUrl>http://diana-hep.org</organizationUrl>
    </developer>
  </developers>

  <scm>
    <connection>scm:git:git@github.com:histogrammar/histogrammar-scala.git</connection>
    <developerConnection>scm:git:git@github.com:histogrammar/histogrammar-scala.git</developerConnection>
    <url>git@github.com:histogrammar/histogrammar-scala.git</url>
  </scm>

{profiles}  <reporting>
    <plugins>
      <plugin>
        <groupId>org.scala-tools</groupId>
        <artifactId>maven-scala-plugin</artifactId>
        <version>2.15.2</version>
      </plugin>
    </plugins>
  </reporting>

  <properties>
    <encoding>UTF-8</encoding>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
{javaversion}  </properties>

{dependencies}
  <repositories>
    <repository>
      <id>central</id>
      <name>Central Repository</name>
      <url>http://repo1.maven.org/maven2</url>
      <layout>default</layout>
      <snapshots>
        <enabled>false</enabled>
      </snapshots>
    </repository>
  </repositories>

  <pluginRepositories>
    <pluginRepository>
      <id>central</id>
      <name>Maven Plugin Repository</name>
      <url>http://repo1.maven.org/maven2</url>
      <layout>default</layout>
      <snapshots>
        <enabled>false</enabled>
      </snapshots>
      <releases>
        <updatePolicy>never</updatePolicy>
      </releases>
    </pluginRepository>
  </pluginRepositories>

  <build>
    <plugins>

      <plugin>
        <!-- see http://davidb.github.com/scala-maven-plugin -->
        <groupId>net.alchim31.maven</groupId>
        <artifactId>scala-maven-plugin</artifactId>
        <version>3.2.2</version>
        <executions>
          <execution>
            <goals>
              <goal>compile</goal>
              <goal>testCompile</goal>
            </goals>
            <configuration>
              <args>
                <arg>-Dscalac.patmat.analysisBudget=512</arg>
                <arg>-deprecation</arg>
                <arg>-feature</arg>
                <arg>-unchecked</arg>
                <arg>-dependencyfile</arg>
                <arg>${{project.build.directory}}/.scala_dependencies</arg>
              </args>
              <recompileMode>incremental</recompileMode>
              <!-- <useZincServer>true</useZincServer> -->
            </configuration>
          </execution>
{sourcejar}{javadocjar}
        </executions>
      </plugin>

{scalatest}{copydependencies}
      <plugin>
        <artifactId>maven-install-plugin</artifactId>
        <version>2.5.2</version>
        <configuration>
          <createChecksum>true</createChecksum>
        </configuration>
      </plugin>

      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-source-plugin</artifactId>
        <version>2.2.1</version>
        <executions>
          <execution>
            <id>attach-sources</id>
            <goals>
              <goal>jar-no-fork</goal>
            </goals>
          </execution>
        </executions>
      </plugin>

{gpgplugin}{stagingplugin}    </plugins>

{pluginmanagement}    <resources>
    </resources>

    <testResources>
    </testResources>
  </build>

{distributionmanagement}</project>
'''

if __name__ == "__main__":
    open("core/pom.xml", "w").write(template.format(
        name = "histogrammar",
        description = "Histogram abstraction to simplify complex aggregations in distributed environments.",
        artifactid = "histogrammar_${scala.binary.version}",
        version = VERSION,
        profiles = profiles,
        javaversion = "",
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>${scala.version}</version>
    </dependency>

    <dependency>
      <groupId>org.scalatest</groupId>
      <artifactId>scalatest_${scala.binary.version}</artifactId>
      <version>2.2.5</version>
      <scope>test</scope>
    </dependency>

  </dependencies>
''',
        sourcejar = "",
        javadocjar = "",
        scalatest = scalatest,
        copydependencies = copydependencies,
        gpgplugin = "",
        stagingplugin = "",
        pluginmanagement = "",
        distributionmanagement = ""
        ))

    open("core/deploy-scala-2.10.xml", "w").write(template.format(
        name = "histogrammar",
        description = "Histogram abstraction to simplify complex aggregations in distributed environments.",
        artifactid = "histogrammar_2.10",
        version = VERSION,
        profiles = "",
        javaversion = javaversion17,
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>2.10.6</version>
    </dependency>

    <dependency>
      <groupId>org.scalatest</groupId>
      <artifactId>scalatest_2.10</artifactId>
      <version>2.2.5</version>
      <scope>test</scope>
    </dependency>

  </dependencies>
''',
        sourcejar = sourcejar,
        javadocjar = javadocjar,
        scalatest = scalatest,
        copydependencies = "",
        gpgplugin = gpgplugin,
        stagingplugin = stagingplugin,
        pluginmanagement = pluginmanagement,
        distributionmanagement = distributionmanagement
        ))

    open("core/deploy-scala-2.11.xml", "w").write(template.format(
        name = "histogrammar",
        description = "Histogram abstraction to simplify complex aggregations in distributed environments.",
        artifactid = "histogrammar_2.11",
        version = VERSION,
        profiles = "",
        javaversion = javaversion18,
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>2.11.8</version>
    </dependency>

    <dependency>
      <groupId>org.scalatest</groupId>
      <artifactId>scalatest_2.11</artifactId>
      <version>2.2.5</version>
      <scope>test</scope>
    </dependency>

  </dependencies>
''',
        sourcejar = sourcejar,
        javadocjar = javadocjar,
        scalatest = scalatest,
        copydependencies = "",
        gpgplugin = gpgplugin,
        stagingplugin = stagingplugin,
        pluginmanagement = pluginmanagement,
        distributionmanagement = distributionmanagement
        ))

    open("sparksql/pom.xml", "w").write(template.format(
        name = "histogrammar-sparksql",
        description = "Adapter for using Histogrammar in SparkSQL.",
        artifactid = "histogrammar-sparksql_${scala.binary.version}",
        version = VERSION,
        profiles = profiles,
        javaversion = "",
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>${{scala.version}}</version>
    </dependency>

    <dependency>
      <groupId>org.diana-hep</groupId>
      <artifactId>histogrammar_${{scala.binary.version}}</artifactId>
      <version>{version}</version>
    </dependency>

    <dependency>
      <groupId>org.apache.spark</groupId>
      <artifactId>spark-sql_${{scala.binary.version}}</artifactId>
      <version>${{spark.version}}</version>
      <scope>provided</scope>
    </dependency>

  </dependencies>
'''.format(version = VERSION),
        sourcejar = "",
        javadocjar = "",
        scalatest = "",
        copydependencies = copydependencies,
        gpgplugin = "",
        stagingplugin = "",
        pluginmanagement = "",
        distributionmanagement = ""
        ))

    open("sparksql/deploy-scala-2.10.xml", "w").write(template.format(
        name = "histogrammar-sparksql",
        description = "Adapter for using Histogrammar in SparkSQL.",
        artifactid = "histogrammar-sparksql_2.10",
        version = VERSION,
        profiles = "",
        javaversion = javaversion17,
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>2.10.6</version>
    </dependency>

    <dependency>
      <groupId>org.diana-hep</groupId>
      <artifactId>histogrammar_2.10</artifactId>
      <version>{version}</version>
    </dependency>

    <dependency>
      <groupId>org.apache.spark</groupId>
      <artifactId>spark-sql_2.10</artifactId>
      <version>1.6.2</version>
      <scope>provided</scope>
    </dependency>

  </dependencies>
'''.format(version = VERSION),
        sourcejar = sourcejar,
        javadocjar = javadocjar,
        scalatest = "",
        copydependencies = "",
        gpgplugin = gpgplugin,
        stagingplugin = stagingplugin,
        pluginmanagement = pluginmanagement,
        distributionmanagement = distributionmanagement
        ))

    open("sparksql/deploy-scala-2.11.xml", "w").write(template.format(
        name = "histogrammar-sparksql",
        description = "Adapter for using Histogrammar in SparkSQL.",
        artifactid = "histogrammar-sparksql_2.11",
        version = VERSION,
        profiles = "",
        javaversion = javaversion18,
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>2.11.8</version>
    </dependency>

    <dependency>
      <groupId>org.diana-hep</groupId>
      <artifactId>histogrammar_2.11</artifactId>
      <version>{version}</version>
    </dependency>

    <dependency>
      <groupId>org.apache.spark</groupId>
      <artifactId>spark-sql_2.11</artifactId>
      <version>2.0.0</version>
      <scope>provided</scope>
    </dependency>

  </dependencies>
'''.format(version = VERSION),
        sourcejar = sourcejar,
        javadocjar = javadocjar,
        scalatest = "",
        copydependencies = "",
        gpgplugin = gpgplugin,
        stagingplugin = stagingplugin,
        pluginmanagement = pluginmanagement,
        distributionmanagement = distributionmanagement
        ))

    open("bokeh/pom.xml", "w").write(template.format(
        name = "histogrammar-bokeh",
        description = "Adapter for using Histogrammar to generate Bokeh plots.",
        artifactid = "histogrammar-bokeh_${scala.binary.version}",
        version = VERSION,
        profiles = profiles,
        javaversion = "",
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>${{scala.version}}</version>
    </dependency>

    <dependency>
      <groupId>org.diana-hep</groupId>
      <artifactId>histogrammar_${{scala.binary.version}}</artifactId>
      <version>{version}</version>
    </dependency>

    <dependency>
      <groupId>io.continuum.bokeh</groupId>
      <artifactId>bokeh_${{scala.binary.version}}</artifactId>
      <version>0.7</version>
    </dependency>

  </dependencies>
'''.format(version = VERSION),
        sourcejar = "",
        javadocjar = "",
        scalatest = "",
        copydependencies = copydependencies,
        gpgplugin = "",
        stagingplugin = "",
        pluginmanagement = "",
        distributionmanagement = ""
        ))

    open("bokeh/deploy-scala-2.10.xml", "w").write(template.format(
        name = "histogrammar-bokeh",
        description = "Adapter for using Histogrammar to generate Bokeh plots.",
        artifactid = "histogrammar-bokeh_2.10",
        version = VERSION,
        profiles = "",
        javaversion = javaversion17,
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>2.10.6</version>
    </dependency>

    <dependency>
      <groupId>org.diana-hep</groupId>
      <artifactId>histogrammar_2.10</artifactId>
      <version>{version}</version>
    </dependency>

    <dependency>
      <groupId>io.continuum.bokeh</groupId>
      <artifactId>bokeh_2.10</artifactId>
      <version>0.7</version>
    </dependency>

  </dependencies>
'''.format(version = VERSION),
        sourcejar = sourcejar,
        javadocjar = javadocjar,
        scalatest = "",
        copydependencies = "",
        gpgplugin = gpgplugin,
        stagingplugin = stagingplugin,
        pluginmanagement = pluginmanagement,
        distributionmanagement = distributionmanagement
        ))

    open("bokeh/deploy-scala-2.11.xml", "w").write(template.format(
        name = "histogrammar-bokeh",
        description = "Adapter for using Histogrammar to generate Bokeh plots.",
        artifactid = "histogrammar-bokeh_2.11",
        version = VERSION,
        profiles = "",
        javaversion = javaversion18,
        dependencies = '''  <dependencies>
    <dependency>
      <groupId>org.scala-lang</groupId>
      <artifactId>scala-library</artifactId>
      <version>2.11.8</version>
    </dependency>

    <dependency>
      <groupId>org.diana-hep</groupId>
      <artifactId>histogrammar_2.11</artifactId>
      <version>{version}</version>
    </dependency>

    <dependency>
      <groupId>io.continuum.bokeh</groupId>
      <artifactId>bokeh_2.11</artifactId>
      <version>0.7</version>
    </dependency>

  </dependencies>
'''.format(version = VERSION),
        sourcejar = sourcejar,
        javadocjar = javadocjar,
        scalatest = "",
        copydependencies = "",
        gpgplugin = gpgplugin,
        stagingplugin = stagingplugin,
        pluginmanagement = pluginmanagement,
        distributionmanagement = distributionmanagement
        ))
