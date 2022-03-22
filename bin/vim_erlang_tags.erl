#!/usr/bin/env escript
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

%%% This script creates a tags file that can be used by Vim.
%%%
%%% See more information in the {@link print_help/0} function.

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
%%%     TagName :: binary(),
%%%     FilePath :: binary(),
%%%     Scope :: scope()
%%%     Kind :: char()
%%%     TagAddress :: tag_address_binary()
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

% 'compile' mode gives better error messages if the script throws an error.
-mode(compile).

-include_lib("kernel/include/file.hrl").

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(COMPILE, fun(Re) ->
                         {ok, CRE} = re:compile(Re, [multiline]),
                         CRE
                 end).

-define(RE_FUNCTIONS,
        ?COMPILE("^([a-z][a-zA-Z0-9_@]*)\\s*\\(")).

-define(RE_TYPESPECS1,
        ?COMPILE("^-\\s*(type|opaque)\\s*([a-zA-Z0-9_@]+)\\b")).

-define(RE_TYPESPECS2,
        ?COMPILE("^-\\s*(type|opaque)\\s*'([^ \\t']+)'")).

-define(RE_DEFINES1,
        ?COMPILE("^-\\s*(record|define)\\s*\\(?\\s*([a-zA-Z0-9_@]+)\\b")).

-define(RE_DEFINES2,
        ?COMPILE("^-\\s*(record|define)\\s*\\(?\\s*'([^ \\t']+)'")).

-define(RE_BUILD,
        ?COMPILE("\\b_build\\b")).

-define(DEFAULT_PATH, ".").

%%%=============================================================================
%%% Types
%%%=============================================================================

-record(parsed_params, {
          include = [] :: [string()],
          ignore  = [] :: [string()],
          output  = [] :: [string()],
          follow  = false :: boolean(),
          otp     = false :: boolean(),
          verbose = false :: boolean(),
          help    = false :: boolean()
         }).

-type parsed_params() :: #parsed_params{}.
%% The parameters of the script after they are parsed.

-record(config, {
          explore :: [file:filename()],
          output  :: file:filename(),
          help    :: boolean()
         }).

-type config() :: #config{}.
%% Configuration that describes what the script needs to do.
%%
%% {@link clean_opts/1}

-type cmd_line_arg() :: string().
%% A command line argument (before parsing).

-type cmd_line_arguments() :: [cmd_line_arg()].
%% A list of command line argument (before parsing).

-type cmd_param() :: include | ignore | output | follow | otp | verbose | help.
%% A command line parameter (after parsing).

-type command_type() :: boolean | stateful.
%% The type of a command line parameter.

-type param_value() :: boolean() |             % if command_type() is boolean
                       [cmd_line_arguments()]. % if command_type() is stateful
%% The value of a command line parameter.

-type tag_address_iolist() :: iolist().
%% Same as {@link tag_address_iolist()} but as an iolist.

-type tag_address_binary() :: binary().
%% The Ex command that positions the cursor on the tag.
%%
%% See `:help tags-file-format' and search for `{tagaddress}'.
%%
%% Example: `/^myfunction\>/'.

-type scope() :: global | local.
%% Shows the scope of a tag. If a tag is local, then the editor should jump to
%% it only from the same file. If a tag is global, then the editor should jump
%% to it from any other file.
%%
%% See `:help tags-file-format' and search for `"file:"'.

%%%=============================================================================
%%% Main function
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc This function is the entry point of the script.
%% @end
%%------------------------------------------------------------------------------
-spec main(Args) -> ok when
      Args :: [string()].
main(Args) ->
    log("Entering main. Args are ~p~n~n", [Args]),
    ParsedArgs = parse_args(#parsed_params{}, Args),
    set_verbose_flag(ParsedArgs),
    Opts = clean_opts(ParsedArgs),
    run(Opts).

%%------------------------------------------------------------------------------
%% @doc Read the files and generate the tags.
%% @end
%%------------------------------------------------------------------------------
-spec run(Config) -> ok when
      Config :: config().
run(#config{help = true}) ->
    print_help();
run(#config{explore = Explore, output = TagFile}) ->
    EtsTags = create_tags(Explore),
    ok = tags_to_file(EtsTags, TagFile),
    ets:delete(EtsTags),
    ok.

%%%=============================================================================
%%% Parse command line arguments
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Parse the the command line arguments.
%% @end
%%------------------------------------------------------------------------------
-spec parse_args(Acc, CmdLineArgs) -> Result when
      Acc :: parsed_params(),
      CmdLineArgs :: cmd_line_arguments(),
      Result :: parsed_params().
parse_args(Opts, []) ->
    Opts;
parse_args(Opts, AllCliArgs) ->
    {Param, RestCliArgs1} = parse_next_arg(AllCliArgs),
    {ParamValue, RestCliArgs2} =
        case get_command_type(Param) of
            boolean ->
                {true, RestCliArgs1};
            stateful ->
                get_full_arg_state(
                  Param, param_get(Param, Opts), RestCliArgs1)
        end,
    parse_args(param_set(Param, ParamValue, Opts), RestCliArgs2).

%%------------------------------------------------------------------------------
%% @doc Parse the next command line argument.
%% @end
%%------------------------------------------------------------------------------
-spec parse_next_arg(AllCliArgs) -> Result when
      AllCliArgs :: nonempty_list(cmd_line_arg()),
      Result :: {cmd_param(),
                 Rest :: cmd_line_arguments()}.
parse_next_arg([Arg | RestArgs] = AllCliArgs) ->
    lists:foldl(
      fun({Param, ParamList}, Acc) ->
              case lists:member(Arg, ParamList) of
                  true -> {Param, RestArgs};
                  _ -> Acc
              end
      end, %% If the parameter is not recognised, just throw it into include
      {include, AllCliArgs},
      allowed_cmd_params()).

%%------------------------------------------------------------------------------
%% @doc Return the type of a command.
%% @end
%%------------------------------------------------------------------------------
-spec get_command_type(Cmd) -> Result when
      Cmd :: cmd_param(),
      Result :: command_type().
get_command_type(C) when C =:= include;
                         C =:= ignore;
                         C =:= output ->
    stateful;
get_command_type(B) when B =:= follow;
                         B =:= otp;
                         B =:= verbose;
                         B =:= help ->
    boolean.

%%------------------------------------------------------------------------------
%% @doc Return the list of allowed command line parameters.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_cmd_params() -> Result when
      Result :: [{cmd_param(), cmd_line_arguments()}].
allowed_cmd_params() ->
    [
     {include, ["-i", "--include", "--"]},
     {ignore,  ["-g", "--ignore"]},
     {output,  ["-o", "--output"]},
     {follow,  ["--follow"]},
     {otp,     ["-p", "--otp"]},
     {verbose, ["-v", "--verbose"]},
     {help,    ["-h", "--help"]}
    ].

%%------------------------------------------------------------------------------
%% @doc Return arguments of the current parameter.
%% @end
%%------------------------------------------------------------------------------
-spec get_full_arg_state(Param, CurrentParamValue, RestCliArgs) -> Result when
      Param :: cmd_param(),
      CurrentParamValue :: cmd_line_arguments(),
      RestCliArgs :: cmd_line_arguments(),
      Result :: {NewParamState :: cmd_line_arguments(),
                 RestCliArgs :: cmd_line_arguments()}.
get_full_arg_state(Param, CurrentParamValue, RestCliArgs) ->
    log("Parsing args for parameter ~p~n", [Param]),
    {NewArgs, Rest} = consume_until_new_command(RestCliArgs),
    case NewArgs of
        [] -> log_error("Arguments needed for ~s.~n", [Param]);
        _ -> ok
    end,
    {NewArgs ++ CurrentParamValue, Rest}.

%%------------------------------------------------------------------------------
%% @doc Consume the arguments until there is a new parameter.
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Get a parameter from the "parsed parameters" record.
%% @end
%%------------------------------------------------------------------------------
-spec param_get(Parameter, ParsedParams) -> Result when
      Parameter :: cmd_param(),
      ParsedParams :: parsed_params(),
      Result :: param_value().
param_get(include, #parsed_params{include = Include}) -> Include;
param_get(ignore, #parsed_params{ignore = Ignore}) -> Ignore;
param_get(output, #parsed_params{output = Output}) -> Output;
param_get(otp, #parsed_params{otp = Otp}) -> Otp;
param_get(verbose, #parsed_params{verbose = Verbose}) -> Verbose;
param_get(help, #parsed_params{help = Help}) -> Help.

%%------------------------------------------------------------------------------
%% @doc Set a parameter in the "parsed parameters" record.
%% @end
%%------------------------------------------------------------------------------
-spec param_set(Parameter, Value, ParsedParams) -> ParsedParams when
      Parameter :: cmd_param(),
      Value :: param_value(),
      ParsedParams :: parsed_params().
param_set(include, Value, PP) -> PP#parsed_params{include = Value};
param_set(ignore, Value, PP) -> PP#parsed_params{ignore = Value};
param_set(output, Value, PP) -> PP#parsed_params{output = Value};
param_set(follow, Value, PP) -> PP#parsed_params{follow = Value};
param_set(otp, Value, PP) -> PP#parsed_params{otp = Value};
param_set(verbose, Value, PP) -> PP#parsed_params{verbose = Value};
param_set(help, Value, PP) -> PP#parsed_params{help = Value}.

%%%=============================================================================
%%% Apply command line parameters
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Set "verbose" mode according to the CLI parameters.
%% @end
%%------------------------------------------------------------------------------
-spec set_verbose_flag(ParsedParams) -> ok when
      ParsedParams :: parsed_params().
set_verbose_flag(#parsed_params{verbose = Verbose}) ->
    put(verbose, Verbose),
    log("Verbose mode on.~n").

%%------------------------------------------------------------------------------
%% @doc Convert the "parsed parameters" record into the "config" record.
%% @end
%%------------------------------------------------------------------------------
-spec clean_opts(ParsedParams) -> Result when
      ParsedParams :: parsed_params(),
      Result :: config().
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
clean_opts(#parsed_params{include = Included,
                          ignore = Ignored,
                          output = [Output],
                          follow = FollowSymLinks}) ->
    log("Set includes to default current dir.~n"),
    #config{explore = to_explore_as_include_minus_ignored(
                        Included, Ignored, FollowSymLinks),
      output = Output}.

%%------------------------------------------------------------------------------
%% @doc Expand all the paths given in included and in ignored to actual
%% filenames, and then subtracts the excluded ones from the included.
%% @end
%%------------------------------------------------------------------------------
-spec to_explore_as_include_minus_ignored(Included, Ignored,
                                          FollowSymLinks) -> Result when
      Included :: [string()],
      Ignored :: [string()],
      FollowSymLinks :: boolean(),
      Result :: [file:filename()].
to_explore_as_include_minus_ignored(Included, Ignored, FollowSymLinks) ->
    AllIncluded = lists:append(expand_dirs(Included, FollowSymLinks)),
    AllIgnored = lists:append(expand_dirs(Ignored, FollowSymLinks)),
    lists:subtract(AllIncluded, AllIgnored).

%%------------------------------------------------------------------------------
%% @doc Return all Erlang source files under the directories (recursively).
%%
%% The regular files are simply returned.
%% @end
%%------------------------------------------------------------------------------
-spec expand_dirs(DirOrFileNames, FollowSymLinks) -> Result when
      DirOrFileNames :: [string()],
      FollowSymLinks :: boolean(),
      Result :: [file:filename()].
expand_dirs(DirOrFilenames, FollowSymLinks) ->
    lists:map(fun(DirOrFilename) ->
                      expand_dirs_or_filenames(DirOrFilename, FollowSymLinks)
              end, DirOrFilenames).

%%------------------------------------------------------------------------------
%% @doc Return all Erlang source files under a directory (recursively).
%%
%% If a file is given, return that file.
%% @end
%%------------------------------------------------------------------------------
-spec expand_dirs_or_filenames(DirOrFileName, FollowSymLinks) -> Result when
      DirOrFileName :: string(),
      FollowSymLinks :: boolean(),
      Result :: [file:filename()].
expand_dirs_or_filenames(DirOrFileName, FollowSymLinks) ->
    case {filelib:is_regular(DirOrFileName),
          filelib:is_dir(DirOrFileName)} of
        {true, false} ->
            % It's a file -> return the file
            [DirOrFileName];
        {false, true} when FollowSymLinks ->
            % It's a directory -> return all source files inside the directory.
            %
            % Using '**' has an advantage over a simple recursive function that
            % it limits directory depths, which ensures that we eventually
            % terminate even if these are symlink loops.
            filelib:wildcard(DirOrFileName ++ "/**/*.{erl,hrl}");
        {false, true} when not FollowSymLinks ->
            % It's a directory -> return all source files inside the directory
            find_source_files(DirOrFileName);
        {false, false} ->
            case filelib:wildcard(DirOrFileName) of
                [] ->
                    % It's neither a file, nor a directory, not a wildcard ->
                    % error
                    log_error("File \"~p\" is not a proper file.~n", [DirOrFileName]),
                    [];
                [_|_] = Filenames ->
                    % It's a wildcard -> expand it
                    lists:append(expand_dirs(Filenames, FollowSymLinks))
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Return all *.erl and *.hrl files in the given directory.
%%
%% Symbolic links are *not* followed.
%% @end
%%------------------------------------------------------------------------------
-spec find_source_files(Dir) -> Result when
      Dir :: file:name_all(),
      Result :: [file:filename()].
find_source_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, FileNames} ->
            lists:append(
              [begin
                   FilePath =
                       case Dir of
                           "." ->
                               % Don't add the './' to the beginning. This way
                               % we behave the same way when the "--follow"
                               % option and thus filelib:wildcard/1 is used.
                               FileName;
                           _ ->
                               filename:join(Dir, FileName)
                       end,
                   case get_file_type(FilePath) of
                       {ok, directory} ->
                           log("Directory found: ~s~n", [FilePath]),
                           find_source_files(FilePath);
                       {ok, regular} ->
                           case filename:extension(FilePath) of
                               Ext when Ext == ".erl";
                                        Ext == ".hrl" ->
                                   log("Source file found: ~s~n", [FilePath]),
                                   case FilePath of
                                       "./" ++ FilePathRest ->
                                           [FilePathRest];
                                       _ ->
                                           [FilePath]
                                   end;
                               _ ->
                                   []
                           end;
                       {ok, _} ->
                           [];
                       {error, Reason} ->
                           log_error("Cannot find file or directory '~s': ~p.~n",
                                     [FilePath, Reason]),
                           []
                   end
               end || FileName <- lists:sort(FileNames)]);
        {error, Reason} ->
            log_error("Cannot read directory '~s': ~p.~n", [Dir, Reason]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc Return the type of the given file.
%% @end
%%------------------------------------------------------------------------------
-spec get_file_type(FileName) -> Result when
      FileName :: file:name_all(),
      Type :: device | directory | other | regular | symlink,
      Result :: {ok, Type} | {error, any()}.
get_file_type(FileName) ->
    case file:read_link_info(FileName) of
        {ok, #file_info{type = FileType}} ->
            {ok, FileType};
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Create tags from directory trees and file lists
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Read the given Erlang source files and return an ets table that contains
%% the appropriate tags.
%% @end
%%------------------------------------------------------------------------------
-spec create_tags(Explore) -> Result when
      Explore :: [file:filename()],
      Result :: ets:tid().
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
-spec add_tags_from_file(File, EtsTags, Verbose) -> ok when
      File :: file:filename(),
      EtsTags :: ets:tid(),
      Verbose :: boolean().
add_tags_from_file(File, EtsTags, Verbose) ->
    put(verbose, Verbose),
    log("~nProcessing file: ~s~n", [File]),

    BaseName = filename:basename(File), % e.g. "mymod.erl"
    ModName = filename:rootname(BaseName), % e.g. "mymod"
    add_file_tag(EtsTags, File, BaseName, ModName),

    case file:read_file(File) of
        {ok, Contents} ->
            ok = scan_tags(Contents, {EtsTags, File, ModName});
        Err ->
            log_error("File ~s not readable: ~p~n", [File, Err])
    end.

%%------------------------------------------------------------------------------
%% @doc Add all tags found in the file to the ETS table.
%% @end
%%------------------------------------------------------------------------------
-spec scan_tags(Contents, {EtsTags, File, ModName}) -> ok when
      Contents :: binary(),
      EtsTags :: ets:tid(),
      File :: file:filename(),
      ModName :: string().
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

%%------------------------------------------------------------------------------
%% @doc Apply a function to the tags that that match a pattern.
%% @end
%%------------------------------------------------------------------------------
-spec scan_tags_core(Contents, Pattern, Fun) -> ok when
      Contents :: binary(),
      Pattern :: re:mp(),
      Fun :: fun().
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

%%------------------------------------------------------------------------------
%% @doc Add a tag about the file.
%%
%% If the file is a module, add a module tag.
%% @end
%%------------------------------------------------------------------------------
-spec add_file_tag(EtsTags, File, BaseName, ModName) -> ok when
      EtsTags :: ets:tid(),
      File :: file:filename(),
      BaseName :: string(),
      ModName :: string().
add_file_tag(EtsTags, File, BaseName, ModName) ->

    % File entry:
    % myfile.hrl <tab> ./myfile.hrl <tab> 1;"  F
    % myfile.erl <tab> ./myfile.erl <tab> 1;"  F
    add_tag(EtsTags, BaseName, File, "1", global, $F),

    case filename:extension(File) of
        ".erl" ->
            % Module entry:
            % myfile <tab> ./myfile.erl <tab> 1;"  M
            add_tag(EtsTags, ModName, File, "1", global, $M);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Add a tag about a function definition.
%% @end
%%------------------------------------------------------------------------------
-spec add_func_tags(EtsTags, File, ModName, FuncName) -> ok when
      EtsTags :: ets:tid(),
      File :: file:filename(),
      ModName :: string(),
      FuncName :: binary().
add_func_tags(EtsTags, File, ModName, FuncName) ->

    log("Function definition found: ~s~n", [FuncName]),

    % Global entry:
    % mymod:f <tab> ./mymod.erl <tab> /^f\>/
    add_tag(EtsTags, [ModName, ":", FuncName], File, ["/^", FuncName, "\\>/"],
            global, $f),

    % Static (or local) entry:
    % f <tab> ./mymod.erl <tab> /^f\>/ <space><space> ;" <tab> file:
    add_tag(EtsTags, FuncName, File, ["/^", FuncName, "\\>/"], local, $f).

%%------------------------------------------------------------------------------
%% @doc Add a tag about a type definition.
%% @end
%%------------------------------------------------------------------------------
-spec add_type_tags(EtsTags, File, ModName, Attribute, TypeName,
                    InnerPattern) -> ok when
      EtsTags :: ets:tid(),
      File :: file:filename(),
      ModName :: string(),
      Attribute :: binary(), % "type" | "opaque"
      TypeName :: binary(),
      InnerPattern :: iolist().
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

%%------------------------------------------------------------------------------
%% @doc Add a tag about a record or macro.
%% @end
%%------------------------------------------------------------------------------
-spec add_record_or_macro_tag(EtsTags, File, Attribute, Name,
                              InnerPattern) -> ok when
      EtsTags :: ets:tid(),
      File :: file:filename(),
      Attribute :: binary(), % "record" | "macro"
      Name :: binary(), % the name of the record or macro
      InnerPattern :: iolist().
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

%%------------------------------------------------------------------------------
%% @doc Add a tags to the ETS table.
%% @end
%%------------------------------------------------------------------------------
-spec add_tag(EtsTags, TagName, File, TagAddress, Scope, Kind) -> ok when
      EtsTags :: ets:tid(),
      TagName :: iodata(),
      File :: file:filename(),
      TagAddress :: tag_address_iolist(),
      Scope :: scope(),
      Kind :: char().
add_tag(EtsTags, TagName, File, TagAddress, Scope, Kind) ->
    _ = ets:insert_new(EtsTags,
                       {{iolist_to_binary(TagName),
                         iolist_to_binary(File),
                         Scope,
                         Kind},
                        iolist_to_binary(TagAddress)}),
    ok.

%%%=============================================================================
%%% Writing tags into a file
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Write the tags into a tag file.
%%
%% See `:help tags-file-format' for the tag file format.
%% @end
%%------------------------------------------------------------------------------
-spec tags_to_file(EtsTags, TagsFile) -> ok when
      EtsTags :: ets:tid(),
      TagsFile :: file:name_all().
tags_to_file(EtsTags, TagsFile) ->
    Header = "!_TAG_FILE_SORTED\t1\t/0=unsorted, 1=sorted/\n",
    TagList = ets:tab2list(EtsTags),
    TagListSorted = lists:sort(fun should_first_tag_come_earlier/2, TagList),
    Entries = [tag_to_binary(Tag) || Tag <- TagListSorted],
    file:write_file(TagsFile, [Header, Entries]),
    ok.

%%------------------------------------------------------------------------------
%% @doc Return whether the first tag should come earlier in the tag file than
%%      the second tag.
%% @end
%%------------------------------------------------------------------------------
-spec should_first_tag_come_earlier(Tag1, Tag2) -> Result when
      Tag1 :: {{TagName, File, Scope, Kind}, TagAddress},
      Tag2 :: {{TagName, File, Scope, Kind}, TagAddress},
      TagName :: binary(),
      File :: binary(),
      Scope :: scope(),
      Kind :: char(),
      TagAddress :: tag_address_binary(),
      Result :: boolean().
should_first_tag_come_earlier(TagA = {{TagName, FilePathA, _, _}, _},
                              TagB = {{TagName, FilePathB, _, _}, _}) ->
    % The two tags have the same TagName, so if one of the tags is in a `_build'
    % directory, that should come later.
    %
    % Explanation: It can happen that the same tag is present in the tag list
    % twice: from the `app' directory and from the `_build' directory.
    %
    % *   On Unix: rebar3 symlinks the applications in the `app` directory into
    %     the `_build' directory. If `vim_erlang_tags.erl' is executed with
    %     `--follow', it follows those symlinks and the tags from the source
    %     files are present in the tag list twice.
    %
    % *   On Windows: rebar3 copies the applications in the `app` directory into
    %     the `_build' directory. The tags from the source files are present in
    %     the tag list twice.
    %
    % If this happens, we want Vim to give precedence to the tags coming from
    % `app'.
    case {is_in_build_dir(FilePathA),
          is_in_build_dir(FilePathB)} of
        {false, true} ->
            % TagA is not in _build dir; so it should be earlier in the tag file
            % than TagB; so it should be earlier in the `Entries 'list; so it
            % should be considered smaller by `lists:sort'; so this function
            % should return `true'.
            true;
        {true, false} ->
            % Opposite reasoning to the previous branch.
            false;
        _ ->
            TagA =< TagB
    end;
should_first_tag_come_earlier(TagA, TagB) ->
    TagA =< TagB.

%%------------------------------------------------------------------------------
%% @doc Return whether FilePath contains a `_build' component.
%% @end
%%------------------------------------------------------------------------------
-spec is_in_build_dir(FilePath) -> Result when
      FilePath :: binary(),
      Result :: boolean().
is_in_build_dir(FilePath) ->
    re:run(FilePath, ?RE_BUILD, [{capture, none}, global]) =/= nomatch.

%%------------------------------------------------------------------------------
%% @doc Convert one tag into a line in a tag file.
%% @end
%%------------------------------------------------------------------------------
-spec tag_to_binary({{TagName, File, Scope, Kind}, TagAddress}) -> Result when
      TagName :: iodata(),
      File :: binary(),
      Scope :: scope(),
      Kind :: char(),
      TagAddress :: tag_address_binary(),
      Result :: binary().
tag_to_binary({{TagName, File, Scope, Kind}, TagAddress}) ->
    ScopeStr =
        case Scope of
            global -> "";
            local -> "\tfile:"
        end,
    iolist_to_binary([TagName, "\t",
                      File, "\t",
                      TagAddress, ";\"\t",
                      Kind,
                      ScopeStr, "\n"]).

%%%=============================================================================
%%% Utility functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Print a log entry.
%% @end
%%------------------------------------------------------------------------------
-spec log(Format) -> ok when
      Format :: io:format().
log(Format) ->
    log(Format, []).

%%------------------------------------------------------------------------------
%% @doc Print a log entry.
%% @end
%%------------------------------------------------------------------------------
-spec log(Format, Data) -> ok when
      Format :: io:format(),
      Data :: [any()].
log(Format, Data) ->
    case get(verbose) of
        true ->
            io:format(Format, Data);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Print an error.
%% @end
%%------------------------------------------------------------------------------
-spec log_error(Format, Data) -> ok when
      Format :: io:format(),
      Data :: [any()].
log_error(Format, Data) ->
    io:format(standard_error, Format, Data).

%%------------------------------------------------------------------------------
%% @doc Print the script's help.
%%
%% This is the last function so that it's easy to jump to it.
%% @end
%%------------------------------------------------------------------------------
-spec print_help() -> ok.
print_help() ->
    Help =
"Usage: vim_erlang_tags.erl [-h|--help] [-v|--verbose] [-o|--output FILE]
                            [-i|--include FILE_WILDCARD]
                            [-g|--ignore FILE_WILDCARD]
                            [--follow] [-p|--otp]
                            DIR_OR_FILE...

Description:
  vim_erlang_tags.erl creates a tags file that can be used by Vim. The
  directories given as arguments are searched (recursively) for *.erl and *.hrl
  files, which will be scanned. The files given as arguments are also scanned.
  The default is to search in the current directory.

Options:
  -h, --help    Print help and exit.
  -v, --verbose Verbose output.
  -o, --output FILE
                Write the output into the given file instead of ./tags.
  -i, --include FILE_WILDCARD
  -g, --ignore FILE_WILDCARD
                Include or ignore the files/directories that match the given wildcard.
                Read http://www.erlang.org/doc/man/filelib.html#wildcard-1 for
                the wildcard patterns.
  --follow      Follow symbolic links
  -p, --otp     Include the currently used OTP lib_dir

Examples:
  $ vim_erlang_tags.erl
  $ vim_erlang_tags.erl .  # Same
  $ vim_erlang_tags.erl /path/to/project1 /path/to/project2
",
    io:format("~s", [Help]).
