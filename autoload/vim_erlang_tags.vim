let s:exec_script = expand('<sfile>:p:h') . "/../bin/vim_erlang_tags.erl"

function! s:GetExecuteCmd()
    let script_opts = ""

    if exists("g:erlang_tags_ignore")
        let ignored_paths = (type(g:erlang_tags_ignore) == type("string") ?
                          \  [ g:erlang_tags_ignore ] :
                          \  g:erlang_tags_ignore)

        for path in ignored_paths
            let script_opts = script_opts . " --ignore " . path
        endfor
    endif

    if exists("g:erlang_tags_outfile") && g:erlang_tags_outfile != ""
        let script_opts = script_opts . " --output " . g:erlang_tags_outfile
    endif

    if exists("g:erlang_tags_follow") && g:erlang_tags_follow == 1
        let script_opts = script_opts . " --follow"
    endif

    if exists("g:erlang_tags_otp") && g:erlang_tags_otp == 1
        let script_opts = script_opts . " --otp"
    endif

    return s:exec_script . script_opts
endfunction

function! vim_erlang_tags#VimErlangTags(...)
    let param = join(a:000, " ")
    let exec_cmd = s:GetExecuteCmd()
    let script_output = system(exec_cmd . " " . param)
    if !v:shell_error
        return 0
    else
        echoerr "vim-erlang-tag failed with: " . script_output
    endif
endfunction

function! vim_erlang_tags#AsyncVimErlangTags(...)
    let param = join(a:000, " ")
    let exec_cmd = s:GetExecuteCmd()
    call system(exec_cmd . " " . param . " " . '&')
endfunction

function! vim_erlang_tags#VimErlangTagsSelect(split)
    if a:split
        split
    endif
    let curr_line = getline('.')
    if curr_line[col('.') - 1] =~# '[#?]'
        normal! w
    endif
    let orig_isk = &isk
    set isk+=:
    normal! "_viwo
    if curr_line[col('.') - 2] =~# '[#?]'
        normal! h
    endif
    let &isk = orig_isk
    let module_marco_start = stridx(curr_line, "?MODULE", col('.') - 1)
    if module_marco_start == col('.') - 1
        " The selected text starts with ?MODULE, so re-select only the
        " function name.
        normal! ov"_viwo
    endif
endfunction

if !exists("s:os")
    if has("win64") || has("win32") || has("win16")
        let s:os = "Windows"
    else
        let s:os = substitute(system('uname'), '\n', '', '')
    endif
endif

" https://vim.fandom.com/wiki/Autocmd_to_update_ctags_file
function! vim_erlang_tags#DelTagOfFile(file)
  let fullpath = a:file
  let cwd = getcwd()
  let tagfilename = cwd . "/tags"
  let f = substitute(fullpath, cwd . "/", "", "")
  let f = escape(f, './')
  let cmd = ""
  if s:os == "Darwin"
      let cmd = 'sed -i "" "/' . f . '/d" "' . tagfilename . '"'
  elseif s:os == "Linux"
      let cmd = 'sed -i "/' . f . '/d" "' . tagfilename . '"'
  endif
  return system(cmd)
endfunction

function! vim_erlang_tags#UpdateTags()
  let f0 = expand("%:p")
  let cwd = getcwd()
  let f = substitute(f0, cwd . "/", "", "")
  let tagfilename = cwd . "/tags"
  let temptags = cwd . "/temptags"
  let exec_cmd = s:GetExecuteCmd()
  call vim_erlang_tags#DelTagOfFile(f)
  let param = " --include " . f . " --output " . temptags
  call vim_erlang_tags#VimErlangTags(param)
  let cmd = "tail -n +2 " . temptags . " | sort -o " . tagfilename . " -m -u " . tagfilename . " - "
  let resp = system(cmd)
endfunction
