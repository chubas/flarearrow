OCAMLC = 'ocamlc'
OCAMLLEX = 'ocamllex'
OCAMLYACC = 'ocamlyacc'

ROOT = Dir.pwd
OUNIT_DIR = '/usr/local/lib/ocaml/3.10.0/oUnit'

def ex(str)
  puts "(>>) #{str}"
  `#{str}`
end

def rm(file)
  if File.exists?(file)
    File.delete(file)
    puts "(~) Deleted #{file}"
  else
    puts "(~) File #{file} doesn't exist"
  end
end

def ocamllex(file, args = '')
  ex "ocamllex #{file}.mll -o #{file}.ml #{args}"
end

def ocamlc(file, includes = '', args = '')
  ex "ocamlc #{includes} #{file}.ml -o #{file}.byte #{args}"
end

namespace :flarearrow do

  desc "Default task"
  task :default do
    puts "Make tests with `rake tests`"
  end

  desc "Build dependencies"
  task :build do
    Dir.chdir(File.join(ROOT, 'src', 'lib'))
    ocamlc('flarelib')
    Dir.chdir(File.join(ROOT, 'src', 'first_level_parser'))
    ocamllex('first_level_parser')
    ocamlc('first_level_parser',
      [ '-I ../lib flarelib.cmo',
        'unix.cma'
      ].join(' ')
    )
    Dir.chdir(File.join(ROOT, 'src', 'tokenizer'))
    ocamlc('exceptions')
    ocamlc('tokenizer',
      [ '-I ../lib flarelib.cmo',
        '-I . exceptions.cmo',
      ].join(' ')
    )
    Dir.chdir(ROOT)
  end

  desc "Run tests"
  task :tests => :build do
    Dir.chdir(File.join(ROOT, 'src', 'first_level_parser'))
    ocamlc('test_first_level_parser',
      [ '-I ../lib flarelib.cmo',
        'unix.cma',
        "-I #{OUNIT_DIR} oUnit.cma",
        'first_level_parser.cmo'
      ].join(' ')
    )
    puts "(==) Running TEST First Level Parser"
    puts ex("./test_first_level_parser.byte")

    Dir.chdir(File.join(ROOT, 'src', 'tokenizer'))
    ocamlc('test_tokenizer',
      [ '-I ../lib flarelib.cmo',
        'unix.cma',
        "-I #{OUNIT_DIR} oUnit.cma",
        "exceptions.cmo",
        "tokenizer.cmo"
      ].join(' ')
    )
    puts "(==) Running TEST Tokenizer"
    puts ex("./test_tokenizer.byte")

    Dir.chdir(ROOT)
  end

  desc "Clean files"
  task :clean do
    ['first_level_parser', 'lib', 'tokenizer'].each do |dir|
      Dir.chdir(File.join(ROOT, 'src', dir))
      `rm *.cmo`
      `rm *.cmi`
      `rm *.byte`
    end
    Dir.chdir(ROOT)
  end

end
