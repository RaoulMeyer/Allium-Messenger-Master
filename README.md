Allium Messenger - Master application
=====

Allium Messenger is a secure and accessible messenger built as a school project. Allium uses Tor-like routing of messages to prevent meta-data of messages leaking, as well as strong encryption to protect the content of messages. This repository contains an integral part of the Allium messenger, the master application. This part of the application is responsible for managing a network of client and so-called node applications. It is built with scalability in mind: it's stateless, uses scalable in-memory databases only and is meant to run in Docker containers to allow for easy up and downscaling.
