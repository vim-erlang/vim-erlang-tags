#!/usr/bin/env escript
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

%%% Copyright 2013 Csaba Hoch
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

%%% The Tags ets table has the following scheme:
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
%%%
%%%         myfile.erl  ./myfile.erl  1;"  F
%%%
%%%     {HrlFileName, FilePath, global, $F} -> TagAddress
%%%
%%%         myfile.hrl  ./myfile.hrl  1;"  F
%%%
%%%     {ModName, FilePath, global, $M} -> TagAddress
%%%
%%%         myfile  ./myfile.erl  1;"  M
%%%
%%%     {FuncName, FilePath, local, $f} -> TagAddress
%%%
%%%         f  ./mymod.erl  /^f\>/;"  f  file:
%%%
%%%     {FuncName, FilePath, global, $f} -> TagAddress
%%%
%%%         mymod:f  ./mymod.erl  /^f\>/;"  f
%%%
%%%     {Type, FilePath, local, $t} -> TagAddress
%%%
%%%         mytype  ./mymod.erl  /^-type\s\*\<mytype\>/;"  t  file:
%%%
%%%     {Type, FilePath, global, $t} -> TagAddress
%%%
%%%         mymod:mytype  ./mymod.erl  /^-type\s\*\<mytype\>/;"  t
%%%
%%%     {Record, FilePath, local, $r} -> TagAddress
%%%
%%%         myrec  ./mymod.erl  /^-record\s\*\<myrec\>/;"  r  file:
%%%
%%%     {Record, FilePath, global, $r} -> TagAddress
%%%
%%%         myrec  ./myhrl.hrl  /^-record\s\*\<myrec\>/;"  r
%%%
%%%     {Macro, FilePath, local, $d} -> TagAddress
%%%
%%%         mymac  ./mymod.erl  /^-define\s\*\<mymac\>/;"  d  file:
%%%
%%%     {Macro, FilePath, global, $d} -> TagAddress
%%%
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

main(Args) ->
    % Process arguments
    put(files, []),
    put(tagsfilename, "tags"),
    put(ignored, []),
    put(incl_otp, false),
    parse_args(Args),
    ArgFiles =
        case get(files) of
            [] ->
                [?DEFAULT_PATH];
            Other ->
                Other
        end,

    Files =
        case get(incl_otp) of
            true -> [code:lib_dir()|ArgFiles];
            false -> ArgFiles
        end,

    Tags = create_tags(Files),
    ok = tags_to_file(Tags, get(tagsfilename)),
    ets:delete(Tags).

%% I know that using the process dictionary is not very nice...
parse_args([]) ->
    ok;
parse_args(["-"|OtherArgs]) ->
    put(files, [stdin|get(files)]),
    parse_args(OtherArgs);
parse_args([Help|_]) when Help == "-h";
                          Help == "--help" ->
    print_help(),
    halt(0);
parse_args([Verbose|OtherArgs]) when Verbose == "-v";
                                     Verbose == "--verbose" ->
    put(verbose, true),
    log("Verbose mode on.~n"),
    parse_args(OtherArgs);
parse_args([InclOTP|OtherArgs]) when InclOTP == "-p";
                                     InclOTP == "--otp" ->
    put(incl_otp, true),
    log("Including OTP in.~n"),
    parse_args(OtherArgs);
parse_args([Output, TagsFileName|OtherArgs]) when Output == "-o";
                                                  Output == "--output" ->
    put(tagsfilename, TagsFileName),
    parse_args(OtherArgs);
parse_args([Output]) when Output == "-o";
                          Output == "--output";
                          Output == "-i";
                          Output == "--ignore" ->
    log_error("More argument needed after ~s.~n", [Output]),
    halt(1);
parse_args([Ignored, Name|OtherArgs]) when Ignored == "-i";
                                           Ignored == "--ignore" ->
    Files = filelib:wildcard(Name),
    AllIgnored = case get(ignored) of
        undefined -> Files;
        OldFiles -> OldFiles ++ [ filename:absname(N, ?DEFAULT_PATH) || N <- Files ]
    end,
    put(ignored, AllIgnored),
    parse_args(OtherArgs);
parse_args(["-" ++ Arg|_]) ->
    log_error("Unknown argument: ~s~n", [Arg]),
    halt(1);
parse_args([FileName|OtherArgs]) ->
    put(files, [FileName|get(files)]),
    parse_args(OtherArgs).

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
  -             Read the list of files from the standard input.
  -o, --output FILE
                Write the output into the given file instead of ./tags.
  -i, --ignore FILE_WILDCARD
                Ignore the files/directories that match the given wildcard.
                Read http://www.erlang.org/doc/man/filelib.html#wildcard-1 for
                the wildcard patterns.
  -p, --otp     Include the currently used OTP lib_dir

Example:
  $ vim-erlang-tags.erl
  $ vim-erlang-tags.erl .  # Same
  $ find . -name '*.[he]rl' | vim-erlang-tags.erl -  # Equivalent to the above
  $ vim-erlang-tags.erl /path/to/project1 /path/to/project2
",
    io:format("~s", [Help]).

%%%=============================================================================
%%% Create tags from directory trees and file lists
%%%=============================================================================

% Read the given Erlang source files and return an ets table that contains the
% appropriate tags.
create_tags(Files) ->
    Tags = ets:new(tags, [ordered_set]),
    log("Tags table created.~n"),

    {StdIn, RealFiles} =
        lists:partition(
          fun(stdin) -> true;
             (_) -> false
          end, Files),

    case StdIn of
        [] ->
            ok;
        _ ->
            process_filenames_from_stdin(Tags)
    end,

    process_filenames(RealFiles, Tags),

    Tags.

% Read file names for stdin and scan the files for tags.
process_filenames_from_stdin(Tags) ->
    case io:get_line(standard_io, "") of
        eof ->
            ok;
        Line ->
            File = trim(Line),
            log("File to process: ~s~n", [File]),
            add_tags_from_file(File, Tags),
            process_filenames_from_stdin(Tags)
    end.

% Traverse the given directory and scan the Erlang files inside for tags.
process_dir_tree(Top, Tags) ->
    IsIgnored = lists:member(Top, get(ignored)),
    if IsIgnored -> ok;
       true ->
            case file:list_dir(Top) of
                {ok, FileNames} ->
                    RelFileNames = [filename:join(Top, FileName) ||
                                    FileName <- FileNames],
                    process_filenames(RelFileNames, Tags);
                {error, eacces} ->
                    log_error("Permission denied: ~s~n", [Top]);
                {error, enoent} ->
                    log_error("Directory does not exist: ~s~n", [Top])
            end
    end.

% Go through the given files: scan the Erlang files for tags and traverse the
% directories for further Erlang files.
process_filenames([], _Tags) ->
    ok;
process_filenames([File|OtherFiles], Tags) ->
    IsIgnored = lists:member(File, get(ignored)),
    if IsIgnored -> ok;
       true ->
            case filelib:is_dir(File) of
                true ->
                    process_dir_tree(File, Tags);
                false ->
                    case filename:extension(File) of
                        Ext when Ext == ".erl";
                                 Ext == ".hrl" ->
                            add_tags_from_file(File, Tags);
                        _ ->
                            ok
                    end
            end
    end,
    process_filenames(OtherFiles, Tags).

%%%=============================================================================
%%% Scan a file or line for tags
%%%=============================================================================

% Read the given Erlang source file and add the appropriate tags to the Tags ets
% table.
add_tags_from_file(File, Tags) ->
    log("~nProcessing file: ~s~n", [File]),

    BaseName = filename:basename(File), % e.g. "mymod.erl"
    ModName = filename:rootname(BaseName), % e.g. "mymod"
    add_file_tag(Tags, File, BaseName, ModName),

    case file:read_file(File) of
        {ok, Contents} -> ok = scan_tags(Contents, {Tags, File, ModName});
        Err -> log_error("File ~s not readable: ~p~n", [File, Err])
    end.

scan_tags(Contents, {Tags, File, ModName}) ->
    scan_tags_core(
      Contents, ?RE_FUNCTIONS,
      fun([_, FuncName]) ->
              add_func_tags(Tags, File, ModName, FuncName)
      end),
    scan_tags_core(
      Contents, ?RE_TYPESPECS1,
      fun([_, Attr, TypeName]) ->
              InnerPattern = [TypeName, "\\>"],
              add_type_tags(Tags, File, ModName, Attr, TypeName, InnerPattern)
      end),
    scan_tags_core(
      Contents, ?RE_TYPESPECS2,
      fun([_, Attr, TypeName]) ->
              InnerPattern = [$', TypeName, $'],
              add_type_tags(Tags, File, ModName, Attr, TypeName, InnerPattern)
      end),
    scan_tags_core(
      Contents, ?RE_DEFINES1,
      fun([_, Attr, Name]) ->
              InnerPattern = [Name, "\\>"],
              add_record_or_macro_tag(Tags, File, Attr, Name, InnerPattern)
      end),
    scan_tags_core(
      Contents, ?RE_DEFINES2,
      fun([_, Attr, Name]) ->
              InnerPattern = [$', Name, $'],
              add_record_or_macro_tag(Tags, File, Attr, Name, InnerPattern)
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

% Add this information to Tags.
add_file_tag(Tags, File, BaseName, ModName) ->

    % myfile.hrl <tab> ./myfile.hrl <tab> 1;"  F
    % myfile.erl <tab> ./myfile.erl <tab> 1;"  F
    % myfile <tab> ./myfile.erl <tab> 1;"  M
    add_tag(Tags, BaseName, File, "1", global, $F),

    case filename:extension(File) of
        ".erl" ->
            add_tag(Tags, ModName, File, "1", global, $M);
        _ ->
            ok
    end.

% File contains the function ModName:FuncName; add this information to Tags.
add_func_tags(Tags, File, ModName, FuncName) ->

    log("Function definition found: ~s~n", [FuncName]),

    % Global entry:
    % mymod:f <tab> ./mymod.erl <tab> /^f\>/
    add_tag(Tags, [ModName, ":", FuncName], File, ["/^", FuncName, "\\>/"],
            global, $f),

    % Static (or local) entry:
    % f <tab> ./mymod.erl <tab> /^f\>/ <space><space> ;" <tab> file:
    add_tag(Tags, FuncName, File, ["/^", FuncName, "\\>/"], local, $f).

% File contains the type ModName:Type; add this information to Tags.
add_type_tags(Tags, File, ModName, Attribute, TypeName, InnerPattern) ->

    log("Type definition found: ~s~n", [TypeName]),

    Pattern = ["/^-\\s\\*", Attribute, "\\s\\*", InnerPattern, $/],

    % Global entry:
    % mymod:mytype <tab> ./mymod.erl <tab> /^-type\s\*mytype\>/
    % mymod:mytype <tab> ./mymod.erl <tab> /^-opaque\s\*mytype\>/
    add_tag(Tags, [ModName, ":", TypeName], File, Pattern, global, $t),

    % Static (or local) entry:
    % mytype <tab> ./mymod.erl <tab> /^-type\s\*mytype\>/
    %     <space><space> ;" <tab> file:
    % mytype <tab> ./mymod.erl <tab> /^-opaque\s\*mytype\>/
    %     <space><space> ;" <tab> file:
    add_tag(Tags, TypeName, File, Pattern, local, $t).

% File contains a macro or record called Name; add this information to Tags.
add_record_or_macro_tag(Tags, File, Attribute, Name, InnerPattern) ->

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
    add_tag(Tags, Name, File,
            ["/^-\\s\\*", Attribute, "\\s\\*(\\?\\s\\*", InnerPattern, "/"],
            Scope, Kind),

    % #myrec  ./mymod.erl  /^-record\s\*\<myrec\>/;"  r  file:
    % #myrec  ./myhrl.hrl  /^-record\s\*\<myrec\>/;"  r
    % ?mymac  ./mymod.erl  /^-define\s\*\<mymac\>/;"  m  file:
    % ?mymac  ./myhrl.hrl  /^-define\s\*\<mymac\>/;"  m
    add_tag(Tags, [Prefix|Name], File,
            ["/^-\\s\\*", Attribute, "\\s\\*(\\?\\s\\*", InnerPattern, "/"],
            Scope, Kind).

add_tag(Tags, Tag, File, TagAddress, Scope, Kind) ->
    ets:insert_new(Tags, {{Tag, File, Scope, Kind}, TagAddress}).

%%%=============================================================================
%%% Writing tags into a file
%%%=============================================================================

tags_to_file(Tags, TagsFile) ->
    Header = "!_TAG_FILE_SORTED\t1\t/0=unsorted, 1=sorted/\n",
    Entries = lists:sort( [ tag_to_binary(Entry) || Entry <- ets:tab2list(Tags) ] ),
    file:write_file(TagsFile, [Header, Entries]),
    ok.

tag_to_binary({{Tag, File, Scope, Kind}, TagAddress}) ->
    ScopeStr =
    case Scope of
        global ->
            "";
        local ->
            "\tfile:"
    end,
    iolist_to_binary( [Tag, "\t",
                       File, "\t",
                       TagAddress, ";\"\t",
                       Kind,
                       ScopeStr, "\n"]).

%%%=============================================================================
%%% Utility functions
%%%=============================================================================

% From http://www.trapexit.org/Trimming_Blanks_from_String
trim(Input) ->
    re:replace(Input, "\\s*$", "", [{return, list}]).

log(Format) ->
    log(Format, []).

log(Format, Data) ->
    case get(verbose) of
        true ->
            io:format(Format, Data);
        _ ->
            ok
    end.

%log_error(Format) ->
%    log_error(Format, []).

log_error(Format, Data) ->
    io:format(standard_error, Format, Data).

