.. _configuration:

=====================
Configuring Pony Mode
=====================

Pony Mode has a number of configuration values that you, the User can set to make your life more pleasant!

Global configuration
====================

There are a number of custom variables...

pony-etags-command
------------------
Command to generate tags table for project

Default: "find . | grep .py | xargs etags"

.. _pony-server-host:

pony-server-host
----------------
Host to run pony dev server

default: "localhost"

.. _pony-server-port:

pony-server-port
----------------
Port to run pony dev server

Default: 8000


pony-settings-module
--------------------
Settings module to use with manage.py

default: "settings"


pony-test-failfast
------------------

Run pony tests with failfast?
Defaultt

pony-sqlite-program
-------------------

Name of the executable to use when running a database REPL for Django
projects using sqlite.
Default: "sqlite3"

pony-snippet-dir
----------------

Directory in which to locate Yasnippet snippets for Pony Mode

Default: /path/to/pony-mode/snippets




Per - Project configuration
----------------------------

Pony projects are defined in the .dir-locals.el file at the root of your current project.

The file should look something like this::

    ;; Pony mode config for the megacorp project
    ((nil ;; This applies these settings regardless of major mode

      (pony-settings . (make-pony-project
                        :python "/home/david/virtualenvs/megacorp/production/bin/python"
                        :settings "local_settings_file")
    )))


