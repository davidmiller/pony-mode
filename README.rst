Django mode for emacs
=====================

New Django mode for emacs.

Features:
---------

* Run dev server in an emacs buffer [C-c C-d r]
  * Checks to see if runserver_plus is available
  * If not uses in-built runserver
* Jump to current project in browser (start server if required) [C-c C-d b]
* Run test case at point in buffer [C-c C-d t]
* Run tests for current app in buffer [C-c C-d t]
* Run Syncdb on current project [C-c C-d m] (migrate)
* South integration - run south convert, schemamigration, migrate
* Run django shell in buffer [C-c C-d s]
  * Checks for shell_plus
  * If not defaults to shell
* run `fab deploy` for current project within emacs [C-c C-d f d]

Buildout Support
----------------

Django mode is aware of buildout, and will use buildout-root/bin/django to
run management commands when available, manage.py when not.

Installation
------------

1. Download django-mode.el
2. Add the following to your .emacs::

    (load-library "path/to/django")
3. Start programming

Roadmap
-------

Check django-mode.org for current todo/wish list
