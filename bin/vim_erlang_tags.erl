#!/usr/bin/env escript
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

%%% Copyright 2013-2020 Csaba Hoch
%%% Copyright 2013 Adam Rutkowski
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

%%% Recommended reading:
%%%
%%% - http://ctags.sourceforge.net/FORMAT
%%% - http://vimdoc.sourceforge.net/htmldoc/tagsrch.html#tags-file-format

%%% The EtsTags ets table has the following scheme:
%%%
%%%     {{TagName, FilePath, Scope, Kind}, TagAddress}
%%%
%%% Or in more readable notation:
%%%
%%%     {TagName, FilePath, Scope, Kind} -> TagAddress
%%%
%%% Examples of entries (and the tags output generated from them):
%%%
%%%     {ErlFileName, FilePath, global, $F} -> TagAddress
%%%         myfile.erl  ./myfile.erl  1;"  F
%%%
%%%     {HrlFileName, FilePath, global, $F} -> TagAddress
%%%         myfile.hrl  ./myfile.hrl  1;"  F
%%%
%%%     {ModName, FilePath, global, $M} -> TagAddress
%%%         myfile  ./myfile.erl  1;"  M
%%%
%%%     {FuncName, FilePath, local, $f} -> TagAddress
%%%         f  ./mymod.erl  /^f\>/;"  f  file:
%%%
%%%     {FuncName, FilePath, global, $f} -> TagAddress
%%%         mymod:f  ./mymod.erl  /^f\>/;"  f
%%%
%%%     {Type, FilePath, local, $t} -> TagAddress
%%%         mytype  ./mymod.erl  /^-type\s\*\<mytype\>/;"  t  file:
%%%
%%%     {Type, FilePath, global, $t} -> TagAddress
%%%         mymod:mytype  ./mymod.erl  /^-type\s\*\<mytype\>/;"  t
%%%
%%%     {Record, FilePath, local, $r} -> TagAddress
%%%         myrec  ./mymod.erl  /^-record\s\*\<myrec\>/;"  r  file:
%%%
%%%     {Record, FilePath, global, $r} -> TagAddress
%%%         myrec  ./myhrl.hrl  /^-record\s\*\<myrec\>/;"  r
%%%
%%%     {Macro, FilePath, local, $d} -> TagAddress
%%%         mymac  ./mymod.erl  /^-define\s\*\<mymac\>/;"  d  file:
%%%
%%%     {Macro, FilePath, global, $d} -> TagAddress
%%%         mymac  ./myhrl.hrl  /^-define\s\*\<mymac\>/;"  d

-mode(compile).

-define(COMPILE, fun(Re) ->
                         {ok, CRE} = re:compile(Re, [multiline]),
                         CRE
                 end).

-define(RE_FUNCTIONS,  ?COMPILE("^([a-z][a-zA-Z0-9_@]*)\\s*\\(")).
-define(RE_TYPESPECS1, ?COMPILE("^-\\s*(type|opaque)\\s*([a-zA-Z0-9_@]+)\\b")).
-define(RE_TYPESPECS2, ?COMPILE("^-\\s*(type|opaque)\\s*'([^ \\t']+)'")).
-define(RE_DEFINES1,   ?COMPILE("^-\\s*(record|define)\\s*\\(?\\s*([a-zA-Z0-9_@]+)\\b")).
-define(RE_DEFINES2,   ?COMPILE("^-\\s*(record|define)\\s*\\(?\\s*'([^ \\t']+)'")).

-define(DEFAULT_PATH, ".").

%%%=============================================================================
%%% Parameter types, maps, defaults
%%%=============================================================================

-record(parsed_params, {
          include = [] :: [string()],
          ignore  = [] :: [string()],
          output  = [] :: [string()],
          otp     = false :: boolean(),
          verbose = false :: boolean(),
          help    = false:: boolean()
         }).

-record(config, {
          explore :: [file:filename()],
          output  :: file:filename(),
          help    :: boolean()}
       ).

-type cmd_param() :: include | ignore | output | otp | verbose | help.
-type cmd_line_arg() :: string().
-type cmd_line_arguments() :: [cmd_line_arg()].
-type parsed_params() :: #parsed_params{}.

-type config() :: #config{}.

-spec allowed_cmd_params() -> [{cmd_param(), cmd_line_arguments()}].
allowed_cmd_params() ->
    [
     {include, ["-i", "--include", "--"]},
     {ignore,  ["-g", "--ignore"]},
     {output,  ["-o", "--output"]},
     {otp,     ["-p", "--otp"]},
     {verbose, ["-v", "--verbose"]},
     {help,    ["-h", "--help"]}
    ].

-type command_type() :: stateful | boolean.

-spec get_command_type(Cmd :: cmd_param()) -> command_type().
get_command_type(C) when C =:= include;
                         C =:= ignore;
                         C =:= output ->
    stateful;
get_command_type(B) when B =:= otp;
                         B =:= verbose;
                         B =:= help ->
    boolean.

main(Args) ->
    log("Entering main. Args are ~p~n~n", [Args]),
    ParsedArgs = reparse_args(#parsed_params{}, Args),
    set_verbose_flag(ParsedArgs),
    Opts = clean_opts(ParsedArgs),
    run(Opts).

run(#config{help = true}) ->
    print_help();
run(#config{explore = Explore, output = TagFile}) ->
    EtsTags = create_tags(Explore),
    ok = tags_to_file(EtsTags, TagFile),
    ets:delete(EtsTags).

set_verbose_flag(#parsed_params{verbose = Verbose}) ->
    put(verbose, Verbose),
    log("Verbose mode on.~n").

-spec reparse_args(parsed_params(), cmd_line_arguments()) -> parsed_params().
reparse_args(Opts, []) ->
    Opts;
reparse_args(Opts, AllArgs) ->
    {Param, ToContinueParsing} = parse_next_arg(AllArgs),
    {ParamState, NextArgs} =
        case get_command_type(Param) of
            boolean ->
                {true, ToContinueParsing};
            stateful ->
                get_full_arg_state(
                  Param, param_get(Param, Opts), ToContinueParsing)
        end,
    reparse_args(param_set(Param, ParamState, Opts), NextArgs).

param_get(include, #parsed_params{include = Include}) -> Include;
param_get(ignore, #parsed_params{ignore = Ignore}) -> Ignore;
param_get(output, #parsed_params{output = Output}) -> Output;
param_get(otp, #parsed_params{otp = Otp}) -> Otp;
param_get(verbose, #parsed_params{verbose = Verbose}) -> Verbose;
param_get(help, #parsed_params{help = Help}) -> Help.

param_set(include, Value, PP) -> PP#parsed_params{include = Value};
param_set(ignore, Value, PP) -> PP#parsed_params{ignore = Value};
param_set(output, Value, PP) -> PP#parsed_params{output = Value};
param_set(otp, Value, PP) -> PP#parsed_params{otp = Value};
param_set(verbose, Value, PP) -> PP#parsed_params{verbose = Value};
param_set(help, Value, PP) -> PP#parsed_params{help = Value}.

-spec parse_next_arg(nonempty_list(cmd_line_arg())) ->
    {cmd_param(), cmd_line_arguments()}.
parse_next_arg([Arg | NextArgs] = AllArgs) ->
    lists:foldl(
      fun({Param, ParamList}, Acc) ->
              case lists:member(Arg, ParamList) of
                  true -> {Param, NextArgs};
                  _ -> Acc
              end
      end, %% If the parameter is not recognised, just throw it into include
      {include, AllArgs},
      allowed_cmd_params()).

%%------------------------------------------------------------------------------
%% @doc Return args for the current parameter, and the rest of the args to
%% continue parsing
%% @end
%%------------------------------------------------------------------------------
-spec get_full_arg_state(Param, CurrentParamState, ToContinueParsing) -> Ret
    when Param :: cmd_param(),
         CurrentParamState :: cmd_line_arguments(),
         ToContinueParsing :: cmd_line_arguments(),
         Ret :: {cmd_line_arguments(), cmd_line_arguments()}.
get_full_arg_state(Param, CurrentParamState, ToContinueParsing) ->
    log("Parsing args for parameter ~p~n", [Param]),
    {StateArgs, Rest} = consume_until_new_command(ToContinueParsing),
    case StateArgs of
        [] -> log_error("Arguments needed for ~s.~n", [Param]);
        _ -> ok
    end,
    {StateArgs ++ CurrentParamState, Rest}.

-spec consume_until_new_command(Args) -> {ConsumedArgs, RestArgs} when
      Args :: cmd_line_arguments(),
      ConsumedArgs :: cmd_line_arguments(),
      RestArgs :: cmd_line_arguments().
consume_until_new_command(Args) ->
    log("    Consuming args ~p~n", [Args]),
    States = lists:foldl(
               fun({_,S}, Acc) -> S ++ Acc end, [], allowed_cmd_params()),
    lists:splitwith(
      fun("-" ++ _ = El) ->
              case lists:member(El, States) of
                  true -> false;
                  _ -> log_error("Unknown argument: ~s~n", [El]), halt(1)
              end;
         (_El) ->
              true
      end, Args).

-spec clean_opts(parsed_params()) -> config().
clean_opts(#parsed_params{help = true}) ->
    #config{help = true};
clean_opts(#parsed_params{include = []} = Opts0) ->
    log("Set includes to default current dir.~n"),
    clean_opts(Opts0#parsed_params{include = [?DEFAULT_PATH]});
clean_opts(#parsed_params{otp = true, include = Inc} = Opts0) ->
    log("Including OTP in.~n"),
    AllIncludes = [code:lib_dir() | Inc],
    clean_opts(Opts0#parsed_params{include = AllIncludes, otp = false});
clean_opts(#parsed_params{output = []} = Opts0) ->
    log("Set output to default 'tags'.~n"),
    clean_opts(Opts0#parsed_params{output = ["tags"]});
clean_opts(#parsed_params{include = Included, ignore = Ignored, output = [Output]}) ->
    log("Set includes to default current dir.~n"),
    #config{explore = to_explore_as_include_minus_ignored(Included, Ignored),
      output = Output}.

%%------------------------------------------------------------------------------
%% @doc Expand all the paths given in included and in ignored to actual
%% filenames, and then subtracts the excluded ones from the included.
%% @end
%%------------------------------------------------------------------------------
-spec to_explore_as_include_minus_ignored([string()], [string()]) ->
    [file:filename()].
to_explore_as_include_minus_ignored(Included, Ignored) ->
    AllIncluded = lists:append(expand_dirs(Included)),
    AllIgnored = lists:append(expand_dirs(Ignored)),
    lists:subtract(AllIncluded, AllIgnored).

-spec expand_dirs([string()]) -> [file:filename()].
expand_dirs(DirOrFilenames) ->
    lists:map(fun expand_dirs_or_filenames/1, DirOrFilenames).

-spec expand_dirs_or_filenames(string()) -> [file:filename()].
expand_dirs_or_filenames(DirOrFileName) ->
    case {filelib:is_regular(DirOrFileName),
          filelib:is_dir(DirOrFileName)} of
        {true, false} ->
            % It's a file -> return the file
            [DirOrFileName];
        {false, true} ->
            % It's a directory -> return all source files inside the directory
            filelib:wildcard(DirOrFileName ++ "/**/*.{erl,hrl}");
        {false, false} ->
            case filelib:wildcard(DirOrFileName) of
                [] ->
                    % It's neither a file, nor a directory, not a wildcard ->
                    % error
                    log_error("File \"~p\" is not a proper file.~n", [DirOrFileName]),
                    [];
                [_|_] = Filenames ->
                    % It's a wildcard -> expand it
                    lists:append(expand_dirs(Filenames))
            end
    end.

%%%=============================================================================
%%% Create tags from directory trees and file lists
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Read the given Erlang source files and return an ets table that contains
%% the appropriate tags.
%% @end
%%------------------------------------------------------------------------------
-spec create_tags([file:filename()]) -> ets:tid().
create_tags(Explore) ->
    log("In create_tags, To explore: ~p~n", [Explore]),
    EtsTags = ets:new(tags,
                      [set,
                       public,
                       {write_concurrency,true},
                       {read_concurrency,false}
                      ]),
    log("EtsTags table created.~n"),
    log("Starting processing of files~n"),
    Processes = process_filenames(Explore, EtsTags, []),
    lists:foreach(
      fun({Pid, Ref}) ->
              receive
                  {'DOWN', Ref, process, Pid, normal} -> ok
              after
                  30000 ->
                      log_error(
                        "Error: scanning the source files took too long.~n",
                        []),
                      halt(1)
              end
      end,
      Processes),
    EtsTags.


%%------------------------------------------------------------------------------
%% @doc Go through the given files: scan the Erlang files for tags.
%%
%% Here we now for sure that `Files` are indeed files with extensions *.erl or
%% *.hrl.
%% @end
%%------------------------------------------------------------------------------
-spec process_filenames(Files, EtsTags, Processes) -> RetProcesses when
      Files :: [file:filename()],
      EtsTags :: ets:tid(),
      Processes :: [{pid(), reference()}],
      RetProcesses :: [{pid(), reference()}].
process_filenames([], _Tags, Processes) ->
    Processes;
process_filenames([File|OtherFiles], EtsTags, Processes) ->
    Verbose = get(verbose),
    P = spawn_monitor(fun() -> add_tags_from_file(File, EtsTags, Verbose) end),
    process_filenames(OtherFiles, EtsTags, [P | Processes]).

%%%=============================================================================
%%% Scan a file or line for tags
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Read the given Erlang source file and add the appropriate tags to the
%%      EtsTags ets table.
%% @end
%%------------------------------------------------------------------------------
add_tags_from_file(File, EtsTags, Verbose) ->
    put(verbose, Verbose),
    log("~nProcessing file: ~s~n", [File]),

    BaseName = filename:basename(File), % e.g. "mymod.erl"
    ModName = filename:rootname(BaseName), % e.g. "mymod"
    add_file_tag(EtsTags, File, BaseName, ModName),

    case file:read_file(File) of
        {ok, Contents} -> ok = scan_tags(Contents, {EtsTags, File, ModName});
        Err -> log_error("File ~s not readable: ~p~n", [File, Err])
    end.

scan_tags(Contents, {EtsTags, File, ModName}) ->
    scan_tags_core(
      Contents, ?RE_FUNCTIONS,
      fun([_, FuncName]) ->
              add_func_tags(EtsTags, File, ModName, FuncName)
      end),
    scan_tags_core(
      Contents, ?RE_TYPESPECS1,
      fun([_, Attr, TypeName]) ->
              InnerPattern = [TypeName, "\\>"],
              add_type_tags(EtsTags, File, ModName, Attr, TypeName, InnerPattern)
      end),
    scan_tags_core(
      Contents, ?RE_TYPESPECS2,
      fun([_, Attr, TypeName]) ->
              InnerPattern = [$', TypeName, $'],
              add_type_tags(EtsTags, File, ModName, Attr, TypeName, InnerPattern)
      end),
    scan_tags_core(
      Contents, ?RE_DEFINES1,
      fun([_, Attr, Name]) ->
              InnerPattern = [Name, "\\>"],
              add_record_or_macro_tag(EtsTags, File, Attr, Name, InnerPattern)
      end),
    scan_tags_core(
      Contents, ?RE_DEFINES2,
      fun([_, Attr, Name]) ->
              InnerPattern = [$', Name, $'],
              add_record_or_macro_tag(EtsTags, File, Attr, Name, InnerPattern)
      end),
    ok.

scan_tags_core(Contents, Pattern, Fun) ->
    case re:run(Contents, Pattern, [{capture, all, binary}, global]) of
        nomatch ->
            ok;
        {match, Matches} ->
            lists:foreach(Fun, Matches)
    end.

%%%=============================================================================
%%% Add specific tags
%%%=============================================================================

% Add this information to EtsTags.
add_file_tag(EtsTags, File, BaseName, ModName) ->

    % myfile.hrl <tab> ./myfile.hrl <tab> 1;"  F
    % myfile.erl <tab> ./myfile.erl <tab> 1;"  F
    % myfile <tab> ./myfile.erl <tab> 1;"  M
    add_tag(EtsTags, BaseName, File, "1", global, $F),

    case filename:extension(File) of
        ".erl" ->
            add_tag(EtsTags, ModName, File, "1", global, $M);
        _ ->
            ok
    end.

% File contains the function ModName:FuncName; add this information to EtsTags.
add_func_tags(EtsTags, File, ModName, FuncName) ->

    log("Function definition found: ~s~n", [FuncName]),

    % Global entry:
    % mymod:f <tab> ./mymod.erl <tab> /^f\>/
    add_tag(EtsTags, [ModName, ":", FuncName], File, ["/^", FuncName, "\\>/"],
            global, $f),

    % Static (or local) entry:
    % f <tab> ./mymod.erl <tab> /^f\>/ <space><space> ;" <tab> file:
    add_tag(EtsTags, FuncName, File, ["/^", FuncName, "\\>/"], local, $f).

% File contains the type ModName:Type; add this information to EtsTags.
add_type_tags(EtsTags, File, ModName, Attribute, TypeName, InnerPattern) ->

    log("Type definition found: ~s~n", [TypeName]),

    Pattern = ["/^-\\s\\*", Attribute, "\\s\\*", InnerPattern, $/],

    % Global entry:
    % mymod:mytype <tab> ./mymod.erl <tab> /^-type\s\*mytype\>/
    % mymod:mytype <tab> ./mymod.erl <tab> /^-opaque\s\*mytype\>/
    add_tag(EtsTags, [ModName, ":", TypeName], File, Pattern, global, $t),

    % Static (or local) entry:
    % mytype <tab> ./mymod.erl <tab> /^-type\s\*mytype\>/
    %     <space><space> ;" <tab> file:
    % mytype <tab> ./mymod.erl <tab> /^-opaque\s\*mytype\>/
    %     <space><space> ;" <tab> file:
    add_tag(EtsTags, TypeName, File, Pattern, local, $t).

% File contains a macro or record called Name; add this information to EtsTags.
add_record_or_macro_tag(EtsTags, File, Attribute, Name, InnerPattern) ->

    {Kind, Prefix} =
        case Attribute of
            <<"record">> ->
                log("Record found: ~s~n", [Name]),
                {$r, $#};
            <<"define">> ->
                log("Macro found: ~s~n", [Name]),
                {$d, $?}
        end,

    Scope =
        case filename:extension(File) of
            ".hrl" ->
                global;
            _ ->
                local
        end,

    % myrec  ./mymod.erl  /^-record\s\*\<myrec\>/;"  r  file:
    % myrec  ./myhrl.hrl  /^-record\s\*\<myrec\>/;"  r
    % mymac  ./mymod.erl  /^-define\s\*\<mymac\>/;"  m  file:
    % mymac  ./myhrl.hrl  /^-define\s\*\<mymac\>/;"  m
    add_tag(EtsTags, Name, File,
            ["/^-\\s\\*", Attribute, "\\s\\*(\\?\\s\\*", InnerPattern, "/"],
            Scope, Kind),

    % #myrec  ./mymod.erl  /^-record\s\*\<myrec\>/;"  r  file:
    % #myrec  ./myhrl.hrl  /^-record\s\*\<myrec\>/;"  r
    % ?mymac  ./mymod.erl  /^-define\s\*\<mymac\>/;"  m  file:
    % ?mymac  ./myhrl.hrl  /^-define\s\*\<mymac\>/;"  m
    add_tag(EtsTags, [Prefix|Name], File,
            ["/^-\\s\\*", Attribute, "\\s\\*(\\?\\s\\*", InnerPattern, "/"],
            Scope, Kind).

add_tag(EtsTags, Tag, File, TagAddress, Scope, Kind) ->
    ets:insert_new(EtsTags, {{Tag, File, Scope, Kind}, TagAddress}).

%%%=============================================================================
%%% Writing tags into a file
%%%=============================================================================

tags_to_file(EtsTags, TagsFile) ->
    Header = "!_TAG_FILE_SORTED\t1\t/0=unsorted, 1=sorted/\n",
    Entries = lists:sort(
                [tag_to_binary(Entry) || Entry <- ets:tab2list(EtsTags)]),
    file:write_file(TagsFile, [Header, Entries]),
    ok.

tag_to_binary({{Tag, File, Scope, Kind}, TagAddress}) ->
    ScopeStr =
    case Scope of
        global -> "";
        local -> "\tfile:"
    end,
    iolist_to_binary([Tag, "\t",
                      File, "\t",
                      TagAddress, ";\"\t",
                      Kind,
                      ScopeStr, "\n"]).

%%%=============================================================================
%%% Utility functions
%%%=============================================================================

log(Format) ->
    log(Format, []).
log(Format, Data) ->
    case get(verbose) of
        true ->
            io:format(Format, Data);
        _ ->
            ok
    end.

log_error(Format, Data) ->
    io:format(standard_error, Format, Data).

print_help() ->
    Help =
"Usage: vim-erlang-tags.erl [-h|--help] [-v|--verbose] [-] [-o|--output FILE]
                            DIR_OR_FILE...

Description:
  vim-erlang-tags.erl creates a tags file that can be used by Vim. The
  directories given as arguments are searched (recursively) for *.erl and *.hrl
  files, which will be scanned. The files given as arguments are also scanned.
  The default is to search in the current directory.

Options:
  -h, --help    Print help and exit.
  -v, --verbose Verbose output.
  -o, --output  FILE
                Write the output into the given file instead of ./tags.
  -i, --include FILE_WILDCARD
  -g, --ignore  FILE_WILDCARD
                Include or ignore the files/directories that match the given wildcard.
                Read http://www.erlang.org/doc/man/filelib.html#wildcard-1 for
                the wildcard patterns.
  -p, --otp     Include the currently used OTP lib_dir

Example:
  $ vim_erlang_tags.erl
  $ vim_erlang_tags.erl .  # Same
  $ find . -name '*.[he]rl' | vim_erlang_tags.erl -  # Equivalent to the above
  $ vim_erlang_tags.erl /path/to/project1 /path/to/project2
",
    io:format("~s", [Help]).
