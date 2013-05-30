# -*- coding: utf-8; mode: ruby  -*-

module App
  def self.ln_s(from, to)
    if File.exist?(to)
      if File.symlink?(to)
        if File.readlink(to) != from
          msg = "symlink(#{to} -> #{File.readlink(to)}) is exist."
        else
          msg = "already installed (#{from} -> #{to}) ."
        end
      else
        msg =  "file(#{to}) is exist."
      end
      STDERR.puts msg
    else
      FileUtils.ln_s(from, to)
      STDOUT.puts "symlink #{from} -> #{to}"
    end
  end
end

desc "設定ファイルのシンボリックリンクの作成"
task :install do
  App.ln_s File.expand_path('xmonad'),   File.expand_path('~/.xmonad')
  App.ln_s File.expand_path('taffybar'), File.expand_path('~/.config/taffybar')
end

DEB_FILE = "deb/xmonad-install-list_1.0_all.deb"
desc "debファイルの作成"
task :deb => DEB_FILE
file DEB_FILE => ["deb/xmonad-install-list",
              "deb/xmonad-startup.desktop",
              "deb/xmonad.start"] do
  cd 'deb' do
    sh 'equivs-build xmonad-install-list'
  end
end

desc "install deb"
task :deb_install => DEB_FILE do
  sh "sudo gdebi #{DEB_FILE}"
end
