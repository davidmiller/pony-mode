VERS = %x[grep -P -o '(?<=Version: )[0-9.a-d]+$' src/pony-mode.el].strip
RELEASEDIR = "pony-mode/#{VERS}"
BUILDDIR = "build/#{RELEASEDIR}"

task :clean do
  sh "rm - rf build"
end

task :tag do
  sh "git tag #{VERS}"
end

task :package do
  p VERS
  p RELEASEDIR
  p BUILDDIR
  sh "mkdir -p #{BUILDDIR}"
  %w[pony-mode.el pony-mode-pkg.el pony-tpl.el snippets].each do |f|
    sh "cp -rv src/#{f} #{BUILDDIR}"
  end
  sh "cp README.rst #{BUILDDIR}/README"
  sh "cd build; tar -cf pony-mode-#{VERS}.tar #{RELEASEDIR}"
end

task :test do
  p "Running unit tests"
  sh " emacs -batch -L src -L tests -L tests/resources -l ert.el -l pony-test.el -f ert-run-tests-batch-and-exit"
end
