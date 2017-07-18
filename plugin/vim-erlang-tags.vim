" Copyright 2013 Csaba Hoch
" Copyright 2013 Adam Rutkowski
"
" Licensed under the Apache License, Version 2.0 (the "License");
" you may not use this file except in compliance with the License.
" You may obtain a copy of the License at
"
"     http://www.apache.org/licenses/LICENSE-2.0
"
" Unless required by applicable law or agreed to in writing, software
" distributed under the License is distributed on an "AS IS" BASIS,
" WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
" See the License for the specific language governing permissions and
" limitations under the License.

if exists("b:vim_erlang_tags_loaded")
    finish
else
    let b:vim_erlang_tags_loaded = 1
endif

autocmd FileType erlang call VimErlangTagsDefineMappings()

let s:exec_script = expand('<sfile>:p:h') . "/../bin/vim-erlang-tags.erl"

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
    return s:exec_script . script_opts
endfunction

function! VimErlangTags()
    let exec_cmd = s:GetExecuteCmd()

    let script_output = system(exec_cmd)
    if !v:shell_error
        return 0
    else
        echoerr "vim-erlang-tag " . script_output
    endif
endfunction

function! AsyncVimErlangTags()
    let exec_cmd = s:GetExecuteCmd()
    call system(exec_cmd . '&')
endfunction

command! ErlangTags call VimErlangTags()

if exists("g:erlang_tags_auto_update") && g:erlang_tags_auto_update == 1
    au BufWritePost *.erl,*.hrl call AsyncVimErlangTags()
endif

function! VimErlangTagsSelect(split)
    if a:split
        split
    endif
    let curr_line = getline('.')
    if curr_line[col('.') - 1] =~# '[#?]'
        normal w
    endif
    let orig_isk = &isk
    set isk+=:
    normal "_viwo
    if curr_line[col('.') - 2] =~# '[#?]'
        normal h
    endif
    let &isk = orig_isk
    let module_marco_start = stridx(curr_line, "?MODULE", col('.') - 1)
    if module_marco_start == col('.') - 1
        " The selected text starts with ?MODULE, so re-select only the
        " function name.
        normal ov"_viwo
    endif
endfunction

function! VimErlangTagsDefineMappings()
    nnoremap <buffer> <c-]>         :call VimErlangTagsSelect(0)<cr><c-]>
    nnoremap <buffer> g<LeftMouse>  :call VimErlangTagsSelect(0)<cr><c-]>
    nnoremap <buffer> <c-LeftMouse> :call VimErlangTagsSelect(0)<cr><c-]>
    nnoremap <buffer> g]            :call VimErlangTagsSelect(0)<cr>g]
    nnoremap <buffer> g<c-]>        :call VimErlangTagsSelect(0)<cr>g<c-]>
    nnoremap <buffer> <c-w><c-]>    :call VimErlangTagsSelect(1)<cr><c-]>
    nnoremap <buffer> <c-w>]        :call VimErlangTagsSelect(1)<cr><c-]>
endfunction
