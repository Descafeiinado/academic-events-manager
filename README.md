cd C:\Development\Learning\academic-events-manager
.\gradlew build && java -Dfile.encoding=UTF-8 -jar build\libs\academic-events-manager.jar

# TODO:

- ~~When pressing Ctrl+C in any FormPage, return him to the previous page;~~ cancelado
- ~~Show confirmation for selected Person;~~
- ~~Convert hardcoded form operations to service calls;~~
- ~~Insert people in the event;~~
  - ~~Course can only be selected if the person is a student;~~
- ~~Certificate Generation;~~
  - Maybe we could save the emitted certificates in a database (optional);
- Event report by type and date;
- List and detail events; (optional)