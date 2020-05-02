# Prophet Utilities #
A (simple) java code to retrieve useful information about DSI Prophet '08 and REV2 sysex patches. Currently outputs the name and patch number of a sysex file (the first patch if a complete bank is loaded), so that you don't accidentally overwrite an existing patch when loading sysex files.
# Usage #
 java -jar prophetutils.jar <path/to/your/sysexfile>
# Note to self / How *not* to contribute #
This project is mainly intended to help me test Java functionalities that I don't use on an everyday basis.
In particular, this is intended as a sandbox to use JUnit 5, VSCode and TDD, and to play around with the NIO, Collections and MIDI APIs.
I makes use of lots of things that are probably not the best technical solutions to say the least, yet it does the job I wanted it to do. So feel free to grab the code, learn from it, recode it better/faster/with a real architecture in mind, but don't send PR, as I won't read them.
It serves as a reminder that technical debt arises even if we want to do our best, simply because we have neither the time nor the knowledge to do the absolute best, and coming back to any code later on will prove harmful to one's ego. I hope I'll learn a lot from these mistakes looking back to this code in a few months.
