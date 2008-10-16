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

def ocamlyacc(file)
  str = "ocamlyacc -v #{file}.mly"
  ex str
  ex "rm #{file}.mli"
  str
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
    Dir.chdir(File.join(ROOT, 'src', 'second_level_parser'))
    ocamlc('basic_types')
    ocamlyacc('grammar')
    ocamlc('grammar')
    ocamllex('second_level_parser')
    ocamlc('second_level_parser')
    ocamlc('expression_evaluator',
      [ '-I ../lib flarelib.cmo',
        '-I ../first_level_parser first_level_parser.cmo',
        'basic_types.cmo', 'grammar.cmo', 'second_level_parser.cmo'
      ].join(' ')
    )

    Dir.chdir(File.join(ROOT, 'src', 'web_server'))
    ocamlc('server',
      [
        'unix.cma',
        '-I /usr/lib/ocaml/3.10.0/netcgi2',     # 'netcgi1_compat.cma netcgi.cma',
        '-I /usr/lib/ocaml/3.10.0/netsys',      # 'netsys.cma',
        '-I /usr/lib/ocaml/3.10.0/netstring/',  # 'netstring.cma',
        '-I /usr/lib/ocaml/3.10.0/netplex/',    #'netplex_mt.cmo',
        '-I /usr/lib/ocaml/3.10.0/nethttpd-for-netcgi2/', #'server.ml'
      ].join(' '), '-linkall'
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
    Dir.chdir(File.join(ROOT, 'src', 'second_level_parser'))
    ocamlc('test_expression_evaluator',
      [ '-I ../lib flarelib.cmo',
        'unix.cma',
        "-I #{OUNIT_DIR} oUnit.cma",
        '-I ../first_level_parser first_level_parser.cmo',
        'basic_types.cmo', 'grammar.cmo',
        'second_level_parser.cmo', 'expression_evaluator.cmo' 
      ].join(' ')
    )
    puts "(==) Running TEST Second Level Parser"
    puts ex("./test_expression_evaluator.byte")
  end

  desc "Clean files"
  task :clean do
    ['lib', 'first_level_parser', 'second_level_parser'].each do |dir|
      Dir.chdir(File.join(ROOT, 'src', dir))
      `rm *.cmo`
      `rm *.cmi`
      `rm *.byte`
      `rm *.mli`
      `rm *.output`
      `rm *.annot`
      `rm *.exe`
    end
    Dir.chdir(ROOT)
  end

end
