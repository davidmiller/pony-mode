Pony Mode -a Django mode for emacs
==================================

New Django mode for emacs.

Features (Non-exhaustive):
--------------------------

* Run dev server in an emacs buffer [C-c C-p r]
  * Checks to see if runserver_plus is available
  * If not uses in-built runserver
* Jump to current project in browser (start server if required) [C-c C-p b]
* Run test case at point in buffer [C-c C-p t]
* Run tests for current app in buffer [C-c C-p t]
* Run Syncdb on current project
* Management commands for current project in interactive buffer
* South integration - run south convert, schemamigration, migrate
* Run django shell in buffer [C-c C-p s]
  * Checks for shell_plus
  * If not defaults to shell
* Fabric integration [C-c C-p f]
* Startapp and dumpdata on current project within emacs
* Database integration with Emacs sql-mode interactive buffer [C-c C-c d
* Django Template minor mode with syntax highlighting for django template tags
* Snippet collection for django
* generate tags table for project
* run manage commands in interactive buffer
* Buildout integration
* Generate TAGS table for project to enable quick navigation
* Jump to template at point or from editing view [C-c C-p g t]
* Virtualenv integration

Fabric Integration
------------------

Pony mode will interact with fabric for your current project, building a list of functions to auto-complete, and running commands within a \*fabric\* buffer.

Buildout Support
----------------

Pony mode is aware of buildout, and will use buildout-root/bin/django to
run management commands when available, manage.py when not.

The command M-x django-buildout will re-run buildout for your current project

M-x django-buildout-bin will run a script from your buildout's bin directory in an interactive buffer

Tags
----

Generate a TAGS table for your project with M-x django-tags
The exact command used is customisable through django-etags-command in
M-x customize-group RET django

Tests
-----

Run tests in in an interactive buffer, allowing you to drop out into ipdb/pdb
as required.

Tests will default to --failfast, but you can turn that off with the variable django-test-failfast or set it in
M-x customize-group django

in a test run buffer, C-c C-g will jump to the last file and line indicated by the traceback.

Virtualenv
----------

Pony-mode will check to see if your project is inside a virtualenv. If so, it will use the python
interpreter from the virtualenv to run all django-related management commands.

Virtualenv support assumes that you initialized the virtualenv with something equivalent to

    $ virtualenv my-cool-project [--no-site-packages]

    $ cd my-cool-project

    $ django-admin.py startproject awzm

    $ source ./bin/activate

    $ cd awzm

    $ emacs settings.py

If your setup is different to this, then the implicit Virtualenv detection may fail.

Fear not though! if you add a .ponyrc file to your project root (e.g. the directory with your manage.py in) then you can specify the python interpreter you would like to use for this project.

The file should look something like this:

    ;; Pony mode config for the megacorp project
    (make-pony-project
        :python "/home/david/virtualenvs/megacorp/production/bin/python")

Installation
------------

1. clone this repo somewhere $ git clone http://github.com/davidmiller/pony-mode
2. Byte-compile the file::

    M-x byte-compile-file
3. Add the path to your load-path::

    (add-to-list 'load-path "path/to/pony-mode")
4. Add to your .emacs::

    (require 'pony-mode)
5. Enjoy

Bugs
----

Pony-mode is under active development, so please report any bugs on the github issue tracker

Licence
-------

Totally GPL

Roadmap
-------

Check pony-mode.org for current todo/wish list
