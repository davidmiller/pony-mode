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
* Fabric integration [C-c C-d f]
* Startapp and dumpdata on current project within emacs
* Database integration with Emacs sql-mode interactive buffer [C-c C-c d
* Django Template minor mode with syntax highlighting for django template tags
* Snippet collection for django
* generate tags table for project
* run manage commands in interactive buffer

Fabric Integration
------------------

Django-mode will interact with fabric for your current project, building a list of functions to auto-complete, and running commands within a \*fabric\* buffer.

Buildout Support
----------------

Django mode is aware of buildout, and will use buildout-root/bin/django to
run management commands when available, manage.py when not.

Tests
-----

Run tests in in an interactive buffer, allowing you to drop out into ipdb/pdb
as required.

Tests will default to --failfast, but you can turn that off with the variable django-test-failfast or set it in
M-x customize-group django

in a test run buffer, C-c C-g will jump to the last file and line indicated by the traceback.

Installation
------------

1. clone this repo somewhere $ git clone http://github.com/davidmiller/django-mode
2. Add the following to your .emacs::

    (load-library "path/to/django")
3. Start programming

Roadmap
-------

Check django-mode.org for current todo/wish list
