import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    id("java")
    id("com.gradleup.shadow") version "8.3.6"
}

group = "br.edu.ifba"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()

    gradlePluginPortal()
}

dependencies {
    implementation("org.projectlombok:lombok:1.18.30")
    annotationProcessor("org.projectlombok:lombok:1.18.30")

    testImplementation(platform("org.junit:junit-bom:5.10.0"))
    testImplementation("org.junit.jupiter:junit-jupiter")

    implementation("org.fusesource.jansi:jansi:2.4.1")

    implementation("org.jline:jline-terminal-jansi:3.30.0")
    implementation("org.jline:jline-console-ui:3.30.0")

    implementation("com.google.code.gson:gson:2.10.1")
}

artifacts {
    add("archives", tasks.named("shadowJar"))
}

tasks {
    getByName("build").dependsOn("shadowJar")
    getByName("jar").finalizedBy("shadowJar")

    withType<Jar> { enabled = false }

    withType<JavaCompile> {
        targetCompatibility = "21"
        sourceCompatibility = "21"

        options.encoding = "UTF-8"
        options.compilerArgs as MutableList<String> -= listOf("--release", "21")
    }

    withType<ShadowJar> {
        enabled = true
        configurations = listOf(project.configurations.runtimeClasspath.get())

        archiveBaseName.set(project.name)
        archiveFileName.set("${project.name}.jar")

        mergeServiceFiles()

        manifest {
//            attributes["Main-Class"] = "br.edu.ifba.JLineTest"
            attributes["Main-Class"] = "br.edu.ifba.Main"
        }
    }

    withType<Test> {
        useJUnitPlatform()
        testLogging {
            events("passed", "skipped", "failed")
            showStandardStreams = true
        }
    }
}