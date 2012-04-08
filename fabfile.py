"""
Fab commands for Pony Mode
"""

from fabric.api import task, hosts, local, lcd,  cd, run
from fabric import operations

deadpan = "happenup@deadpansincerity.com"

@task
def make_docs():
    """
    Rebuild the documentation
    """
    with lcd("doc/"):
        local("make html")

@task
@hosts(deadpan)
def upload_docs():
    """
    Build, compress, upload and extract the latest docs
    """
    with lcd("doc/build/html"):
        local("rm -rf pony-mode-docs.tar.gz")
        local("tar zcvf pony-mode-docs.tar.gz *")
        operations.put("pony-mode-docs.tar.gz",
                       "/home/happenup/webapps/pony-mode-docs/pony-mode-docs.tar.gz")
    with cd("/home/happenup/webapps/pony-mode-docs/"):
        run("tar zxvf pony-mode-docs.tar.gz")
