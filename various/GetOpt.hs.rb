def mapn(*ll)
  ll.empty? and return []
  r = []
  ll[0].each_index{|i| r << yield(ll.map{|l| l[i]})}
  r
end
def unzip(l); mapn(*l){|l| l}; end

# scanning mode: one of "at_beginning", "anywhere", "all_options"
# kind: one of "noarg", "arg", "optional_arg"

Options = Struct.new("Options", :short, :long, :kind, :descr)

def usageInfo(opt_descr)
  ll = opt_descr.map{|opts|
    [
      fmt_opt(false, opts.kind, opts.short),
      fmt_opt(true,  opts.kind, opts.long.split),
      opts.descr,
    ]
  }

  max_sizes = unzip(ll).map{|l| l.map{|e| e.length}.max}

  ll.map{|l|
    mapn(l, max_sizes){|e, max_size| 
      "  " + e + " " * (max_size - e.length)
    }.join
  }.join("\n")
end

def fmt_opt(is_long, kind, opts)
  (dash, space, equal) = is_long ? ["--", "", "="] : ["-", " ", ""]

  l = 
    if kind =~ /^arg=(.*)/
      opts.map{|e| dash + e + space + equal + $1}
    elsif kind =~ /optional_arg=(.*)/
      opts.map{|e| dash + e + "[" + equal + $1 + "]" }
    else
      opts.map{|e| dash + e}
    end
  l.join(", ")
end

def get_opt(opt_descr, scanning_mode, *args)
    
  args_in_between, pbs, options = [], [], []

  check_matches = proc{|opt, matches|
    if matches.size == 1
      matches[0]
    else
      pbs << 
	if matches.empty?
	  "unrecognized option `#{opt}'";
	else
	  "option `#{opt}' is ambiguous; could be one of:\n" + usageInfo(matches)
	end
      false
    end
  }

  handle_long = proc{|opt, arg|
    match = check_matches.call(opt, opt_descr.find_all{|e| e.long =~ /\b#{Regexp.quote(opt)}/ }) or break
    kind = match.kind
	
    if kind == 'noarg' && arg
      pbs << "option `#{opt}' doesn't allow an argument"
    elsif kind =~ /^arg=(.*)/ && !arg
      pbs << "option `#{opt}' requires an argument 1"
    else
      options << [ match.long, arg ]
    end
  }
  handle_short = proc{|opts|
    while opts != ""
      opt = opts.slice!(0..0)

      match = check_matches.call(opt, opt_descr.find_all{|e| e.short.include?(opt) }) or next
      kind = match.kind

      if kind == 'noarg'
	arg = ''
      elsif opts
	arg, opts = opts, ''
      elsif kind =~ /^arg=(.*)/
	arg = args.shift or pbs << "option `#{opt}' requires an argument #{$1}" ; break
      end
      options << [ match.long, arg ]
    end
  }

  while args
    $_ = args.shift

    if    ~ /^--$/;         break # if scanning_mode ne 'all_options'
    elsif ~ /^--(.*)=(.*)/; handle_long.call($1, $2)
    elsif ~ /^--(.*)/;      handle_long.call($1, nil)
    elsif ~ /^-(.+)/ ;      handle_short.call($1)
    else 
      args_in_between << $_
      break if scanning_mode == 'at_beginning'
    end
  end
  pbs.empty? or raise pbs.join("\n")

  if scanning_mode == 'all_options'
    options << args_in_between.map{|e| [ 'Arg', e ]}
    args_in_between = []
  end
  return options, args_in_between + args
end

############################################-
# and here a small and hopefully enlightening example:

opt_descr = [
    Options.new('v',  'verbose',         'noarg',             "verbosely list files"),
    Options.new('V?', 'version release', 'noarg',             "show version info"),
    Options.new('o',  'output',          'optional_arg=FILE', "use FILE for dump"),
    Options.new('n',  'name',            'arg=USER',          "only dump USER's files"),
]

begin
  p get_opt(opt_descr, "at_beginning", '-V', 'ee')
rescue
  print "#{$!}\n" + "Usage: foobar [OPTION...] files...\n" + usageInfo(opt_descr)
end
