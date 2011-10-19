VERSION = %x[grep -P -o '(?<=version=)[0-9]+$' pony-mode.el].strip
RELEASEDIR = "pony-mode/#{VERSION}"
BUILDDIR = "build/#{RELEASEDIR}"

task :clean do
  sh "rm - rf build"
end

task :tag do
  sh "git tag #{VERSION}"
end

task :package do
  sh "mkdir -p #{BUILDDIR}"
  %w[pony-mode.el pony-mode-pkg.el snippets].each do |f|
    sh "cp -rv #{f} #{BUILDDIR}"
  end
  sh "cp README.rst #{BUILDDIR}/README"
  sh "cd build; tar -cf pony-mode-#{VERSION}.tar $(RELEASEDIR)"
end
