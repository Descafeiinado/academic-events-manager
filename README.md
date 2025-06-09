# Academic Events Manager

A Java application for managing academic events.

> In the `data/` directory, we have some pre-populated data files that can be used to test the
> application. These files are not required for the application to run but can be useful for
> demonstration purposes.

## Prerequisites

- Java 17 or higher
- Unix-like OS (Linux/macOS) or Windows
- Git (optional, for cloning)

## Getting Started

### 1. Make Gradle Wrapper Executable (UNIX only)

```bash
chmod u+x gradlew
```

### 2. Build the Project

```bash
./gradlew build
```

> On Windows, use:
>
> ```bat
> .\gradlew build
> ```

### 3. Run the Application

```bash
java -Dfile.encoding=UTF-8 -jar build/libs/academic-events-manager.jar
```

> On Windows, use:
>
> ```bat
> java -Dfile.encoding=UTF-8 -jar build\libs\academic-events-manager.jar
> ```

## Notes

* The `-Dfile.encoding=UTF-8` flag ensures consistent character encoding across platforms.
* The final `.jar` file will be located in the `build/libs` directory after a successful build.