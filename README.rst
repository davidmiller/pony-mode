Pony Mode -a Django mode for emacs
==================================

A Django mode for emacs.

Features (Non-exhaustive):
--------------------------

* Run dev server in an emacs buffer [C-c C-p r]
  * Checks to see if runserver_plus is available
  * If not uses in-built runserver
* Jump to current project in browser (start server if required) [C-c C-p b]
* Run test case at point in buffer [C-c C-p t]
* Run tests for current app in buffer [C-c C-p t]
* Run Syncdb on current project
* Run Celeryd [C-c C-p c]
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

Documentation
-------------

The full documentation is available at http://www.deadpansincerity.com/docs/pony/

Installation
------------

1. clone this repo somewhere $ git clone https://github.com/davidmiller/pony-mode
2. (optional) Byte-compile the files::

    M-x byte-compile-file (path/to/pony-mode/src/*.el)
    
3. Add the path to your load-path::

    (add-to-list 'load-path "path/to/pony-mode/src")
4. Add to your .emacs::

    (require 'pony-mode)
5. Enjoy

Bugs
----

Please report any bugs on the github issue tracker

Configuration 
-------------

Configuration options per project are available via .dir-locals.el


The file should look something like this::

    ;; Pony mode config for the megacorp project
    ((nil . ;; This applies these settings regardless of major mode

      ((pony-settings (make-pony-project
                       :python "/home/david/virtualenvs/megacorp/production/bin/python"
                       :pythonpath "/home/david/megacorp/libs/projectzero"
                       :settings "local_settings_file"
                       :appsdir "testproject/apps/")
    ))))

Help
----
Turns out that there is a mailing list at https://groups.google.com/group/pony-mode .

Low frequency, high helpfulness. Feel free to stop by for helps & chats...


Licence
-------

Totally GPL

Roadmap
-------

Check the org-mode file pony.org for current todo/wish list

(If you can stand the org-to-github-markdown transition:
https://github.com/davidmiller/pony-mode/blob/master/pony.org )hs
